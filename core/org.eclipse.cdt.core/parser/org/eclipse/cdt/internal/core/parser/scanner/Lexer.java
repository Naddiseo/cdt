/*******************************************************************************
 * Copyright (c) 2007, 2010 Wind River Systems, Inc. and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *    Markus Schorn - initial API and implementation
 *    Mike Kucera (IBM) - UTF string literals
 *******************************************************************************/ 
package org.eclipse.cdt.internal.core.parser.scanner;

import java.util.ArrayDeque;
import java.util.Deque;

import org.eclipse.cdt.core.parser.IGCCToken;
import org.eclipse.cdt.core.parser.IProblem;
import org.eclipse.cdt.core.parser.IToken;
import org.eclipse.cdt.core.parser.OffsetLimitReachedException;
import org.eclipse.cdt.core.parser.util.CharArrayUtils;

/**
 * In short this class converts line endings (to '\n') and trigraphs 
 * (to their corresponding character), 
 * removes line-splices, comments and whitespace other than newline.
 * Returns preprocessor tokens.
 * <p>
 * In addition to the preprocessor tokens the following tokens may also be returned:
 * {@link #tBEFORE_INPUT}, {@link IToken#tEND_OF_INPUT}, {@link IToken#tCOMPLETION}.
 * <p>
 * Number literals are split up into {@link IToken#tINTEGER} and {@link IToken#tFLOATINGPT}. 
 * No checks are done on the number literals.
 * <p>
 * UNCs are accepted, however characters from outside of the basic source character set are
 * not converted to UNCs. Rather than that they are tested with 
 * {@link Character#isUnicodeIdentifierPart(char)} and may be accepted as part of an 
 * identifier.
 * <p>
 * The characters in string literals and char-literals are left as they are found, no conversion to
 * an execution character-set is performed.
 */
final public class Lexer implements ITokenSequence {
	public static final int tBEFORE_INPUT   = IToken.FIRST_RESERVED_SCANNER;
	public static final int tNEWLINE		= IToken.FIRST_RESERVED_SCANNER + 1;
	public static final int tQUOTE_HEADER_NAME    = IToken.FIRST_RESERVED_SCANNER + 2;
	public static final int tSYSTEM_HEADER_NAME   = IToken.FIRST_RESERVED_SCANNER + 3;
	public static final int tOTHER_CHARACTER 	  = IToken.FIRST_RESERVED_SCANNER + 4;
	
	private static final int END_OF_INPUT = -1;
	private static final int ORIGIN_LEXER = OffsetLimitReachedException.ORIGIN_LEXER;
	
	public final static class LexerOptions implements Cloneable {
		public boolean fSupportDollarInIdentifiers= true;
		public boolean fSupportAtSignInIdentifiers= true;
		public boolean fSupportMinAndMax= true;
		public boolean fCreateImageLocations= true;
		public boolean fSupportSlashPercentComments= false;
		public boolean fSupportUTFLiterals= true;
		
		@Override
		public Object clone() {
			try {
				return super.clone();
			} catch (CloneNotSupportedException e) {
				return null;
			}
		}
	}

	// configuration
	private final LexerOptions fOptions;
	private boolean fSupportContentAssist= false;
	private final ILexerLog fLog;
	private final Object fSource;
	
	// the input to the lexer
	private final AbstractCharArray fInput;
	private int fStart;
	private int fLimit;

	// after phase 3 (newline, trigraph, line-splice)
	private int fOffset;
	private int fEndOffset;
	private int fCharPhase3;
	
	private boolean fInsideIncludeDirective= false;
	private Token fToken;
	private Token fLastToken;
	
	// For the few cases where we have to lookahead more than one character
	private Deque<Integer> fMarkOffset;
	private Deque<Integer> fMarkEndOffset;
	private Deque<Integer> fMarkPrefetchedChar;
	private int fDequeDepth; // Keep track of how many internal save states
	// To store the entire state.
	private boolean fMarkInsideIncludeDirective;
	private Token fMarkToken;
	private Token fMarkLastToken;
	
	// Utility for numbers
	private NumberParser fNumberParser;
	
	public Lexer(char[] input, LexerOptions options, ILexerLog log, Object source) {
		this(new CharArray(input), options, log, source, CharArrayUtils.EMPTY);
	}

	public Lexer(AbstractCharArray input, LexerOptions options, ILexerLog log, Object source) {
		this(input, options, log, source, CharArrayUtils.EMPTY);
	}

	public Lexer(char[] input, LexerOptions options, ILexerLog log, Object source, char[] additionalSuffixes) {
		this(new CharArray(input), 0, input.length, options, log, source, additionalSuffixes);
	}

	public Lexer(AbstractCharArray input, LexerOptions options, ILexerLog log, Object source, char[] additionalSuffixes) {
		this(input, 0, input.tryGetLength(), options, log, source, additionalSuffixes);
	}
	
	public Lexer(AbstractCharArray input, int start, int end, LexerOptions options, ILexerLog log, Object source, char[] additionalSuffixes) {
		fInput= input;
		fStart= fOffset= fEndOffset= start;
		fLimit= end;
		fOptions= options;
		fLog= log;
		fSource= source;
		fLastToken= fToken= new Token(tBEFORE_INPUT, source, start, start);
		fMarkOffset = new ArrayDeque<Integer>();
		fMarkEndOffset = new ArrayDeque<Integer>();
		fMarkPrefetchedChar = new ArrayDeque<Integer>();
		fDequeDepth = 0;
		nextCharPhase3();

		fNumberParser = new NumberParser(fOptions, additionalSuffixes, new NumberParser.CharGetter() {
			@Override
			public int get() {
				return fCharPhase3;
			}

			@Override
			public int next() {
				return nextCharPhase3();
			}

			@Override
			public Token getIdentifier() {
				return identifier(fOffset, 0);
			}

			@Override
			public void mark() {
				markPhase3();
			}

			@Override
			public void restore() {
				restorePhase3();
			}

			@Override
			public int getOffset() {
				return fOffset;
			}

			@Override
			public char[] getSubstring(int start, int length) {
				char[] result = new char[length];
				fInput.arraycopy(start, result, 0, length);
				return result;
			}

			@Override
			public boolean isIdentifierStart() {
				return Lexer.this.isIdentifierStart();
			}

			@Override
			public char[] getCharImage(int offset, int endOffset, int imageLength) {
				return Lexer.this.getCharImage(offset, endOffset, imageLength);
			}
		});
	}
	
	private boolean isValidOffset(int pos) {
		if (fLimit < 0)
			return fInput.isValidOffset(pos);
		
		return pos < fLimit;
	}

	/**
	 * Returns the source that is attached to the tokens generated by this lexer
	 */
	public Object getSource() {
		return fSource;
	}

	/**
	 * Resets the lexer to the first char and prepares for content-assist mode. 
	 */
	public void setContentAssistMode(int offset) {
		fSupportContentAssist= true;
		if (isValidOffset(offset)) {
			fLimit= offset;
		}
		// re-initialize 
		fOffset= fEndOffset= fStart;
		nextCharPhase3();
	}
	
	public boolean isContentAssistMode() {
		return fSupportContentAssist;
	}

	/**
	 * Call this before consuming the name-token in the include directive. It causes the header-file 
	 * tokens to be created. 
	 */
	public void setInsideIncludeDirective(boolean val) {
		fInsideIncludeDirective= val;
	}
	
	/** 
	 * Returns the current preprocessor token, does not advance.
	 */
	@Override
	public Token currentToken() {
		return fToken;
	}

	/**
	 * Returns the endoffset of the token before the current one.
	 */
	@Override
	public int getLastEndOffset() {
		return fLastToken.getEndOffset();
	}

	/**
	 * Advances to the next token, skipping whitespace other than newline.
	 * @throws OffsetLimitReachedException when completion is requested in a literal or a header-name.
	 */
	@Override
	public Token nextToken() throws OffsetLimitReachedException {
		fLastToken= fToken;
		return fToken= fetchToken();
	}

	public boolean currentTokenIsFirstOnLine() {
		final int type= fLastToken.getType();
		return type == tNEWLINE || type == tBEFORE_INPUT;
	}
	
	/**
	 * Advances to the next newline or the end of input. The newline will not be consumed. If the
	 * current token is a newline no action is performed.
	 * Returns the end offset of the last token before the newline. 
	 * @param origin parameter for the {@link OffsetLimitReachedException} when it has to be thrown.
	 * @since 5.0
	 */
	public final int consumeLine(int origin) throws OffsetLimitReachedException {
		Token t= fToken;
		Token lt= null;
		while (true) {
			switch(t.getType()) {
			case IToken.tCOMPLETION:
				if (lt != null) {
					fLastToken= lt;
				}
				fToken= t;
				throw new OffsetLimitReachedException(origin, t);
			case IToken.tEND_OF_INPUT:
				if (fSupportContentAssist) {
					t.setType(IToken.tCOMPLETION);
					throw new OffsetLimitReachedException(origin, t);
				}
				//$FALL-THROUGH$
			case Lexer.tNEWLINE:
				fToken= t;
				if (lt != null) {
					fLastToken= lt;
				}
				return getLastEndOffset();
			}
			lt= t;
			t= fetchToken();
		}
	}

	/** 
	 * Advances to the next pound token that starts a preprocessor directive. 
	 * @return pound token of the directive or end-of-input.
	 * @throws OffsetLimitReachedException when completion is requested in a literal or an header-name.
	 */
	public Token nextDirective() throws OffsetLimitReachedException {
		Token t0;
		Token t1= fToken;
		for(;;) {
			t0= t1;
			t1= fetchToken();
			final int tt1 = t1.getType();
			if (tt1 == IToken.tEND_OF_INPUT)
				break;
			if (tt1 == IToken.tPOUND) {
				final int tt0= t0.getType();
				if (tt0 == tNEWLINE || tt0 == tBEFORE_INPUT)
					break;
			}
		}
		fLastToken= t0;
		return fToken=t1;
	}
	
	/**
	 * Computes the next token.
	 */
	private Token fetchToken() throws OffsetLimitReachedException {
		while (true) {
			markPhase3();
			final int start= fOffset;
			final int c= fCharPhase3;
			final int d= nextCharPhase3();
			switch(c) {
			case END_OF_INPUT:
				return newToken(IToken.tEND_OF_INPUT, start);
			case '\n':
				fInsideIncludeDirective= false;
				return newToken(Lexer.tNEWLINE, start);
			case ' ':
			case '\t':
			case 0xb:  // vertical tab
			case '\f': 
			case '\r':
				popPhase3State();
				continue;

			case 'L':
				switch(d) {
				case 'R':
					markPhase3();
					if (nextCharPhase3() == '"') {
						nextCharPhase3();
						return rawStringLiteral(start, 3, IToken.tLSTRING);
					}
					restorePhase3();
					break;
				case '"':
					nextCharPhase3();
					return stringLiteral(start, 2, IToken.tLSTRING);
				case '\'':
					nextCharPhase3();
					return charLiteral(start, IToken.tLCHAR);
				}
				return identifier(start, 1);

			case 'u': 	
			case 'U':
				if (fOptions.fSupportUTFLiterals) {
					switch(d) {
					case 'R':
						markPhase3();
						if (nextCharPhase3() == '"') {
							nextCharPhase3();
							return rawStringLiteral(start, 3, c == 'u' ? IToken.tUTF16STRING : IToken.tUTF32STRING);
						}
						restorePhase3();
						break;
					case '"':
						nextCharPhase3();
						return stringLiteral(start, 2, c == 'u' ? IToken.tUTF16STRING : IToken.tUTF32STRING);
					case '\'':
						nextCharPhase3();
						return charLiteral(start, c == 'u' ? IToken.tUTF16CHAR : IToken.tUTF32CHAR);
					case '8':
						if (c == 'u') {
							markPhase3();
							switch (nextCharPhase3()) {
							case 'R':
								if (nextCharPhase3() == '"') {
									nextCharPhase3();
									return rawStringLiteral(start, 4, IToken.tSTRING);
								}
								break;
							case '"':
								nextCharPhase3();
								return stringLiteral(start, 3, IToken.tSTRING);
							}
							restorePhase3();
						}
						break;
					}
				}
				return identifier(start, 1);
				
			case 'R':
				if (d == '"') {
					nextCharPhase3();
					return rawStringLiteral(start, 2, IToken.tSTRING);
				}
				return identifier(start, 1);
				
			case '"':
				if (fInsideIncludeDirective) {
					return headerName(start, true);
				}
				return stringLiteral(start, 1, IToken.tSTRING);

			case '\'':
				return charLiteral(start, IToken.tCHAR);

			case 'a': case 'b': case 'c': case 'd': case 'e': case 'f': case 'g': case 'h': case 'i': 
			case 'j': case 'k': case 'l': case 'm': case 'n': case 'o': case 'p': case 'q': case 'r': 
			case 's': case 't':           case 'v': case 'w': case 'x': case 'y': case 'z':
			case 'A': case 'B': case 'C': case 'D': case 'E': case 'F': case 'G': case 'H': case 'I':
			case 'J': case 'K':           case 'M': case 'N': case 'O': case 'P': case 'Q':  
			case 'S': case 'T':           case 'V': case 'W': case 'X': case 'Y': case 'Z':
			case '_':
				return identifier(start, 1);

			case '$':
				if (fOptions.fSupportDollarInIdentifiers) {
					return identifier(start, 1);
				}
				break;
			case '@':
				if (fOptions.fSupportAtSignInIdentifiers) {
					return identifier(start, 1);
				}
				break;

			case '\\':
				switch(d) {
				case 'u': case 'U':
					nextCharPhase3();
					return identifier(start, 2);
				}
				return newToken(tOTHER_CHARACTER, start, 1);

			case '0': case '1': case '2': case '3': case '4':
			case '5': case '6': case '7': case '8': case '9':
				restorePhase3();
				return number();

			case '.':
				switch(d) {
				case '0': case '1': case '2': case '3': case '4':
				case '5': case '6': case '7': case '8': case '9':
					restorePhase3();
					return number();

				case '.':
					markPhase3();
					if (nextCharPhase3() == '.') {
						nextCharPhase3();
						return newToken(IToken.tELLIPSIS, start);
					}
					restorePhase3();
					break;

				case '*':
					nextCharPhase3();
					return newToken(IToken.tDOTSTAR, start);
				}
				return newToken(IToken.tDOT, start);

			case '#':
				if (d == '#') {
					nextCharPhase3();
					return newToken(IToken.tPOUNDPOUND, start);
				}
				return newToken(IToken.tPOUND, start);

			case '{':
				return newToken(IToken.tLBRACE, start);
			case '}':
				return newToken(IToken.tRBRACE, start);
			case '[':
				return newToken(IToken.tLBRACKET, start);
			case ']':
				return newToken(IToken.tRBRACKET, start);
			case '(':
				return newToken(IToken.tLPAREN, start);
			case ')':
				return newToken(IToken.tRPAREN, start);
			case ';':
				return newToken(IToken.tSEMI, start);

			case ':':
				switch(d) {
				case ':':
					nextCharPhase3();
					return newToken(IToken.tCOLONCOLON, start);
				case '>': 
					nextCharPhase3();
					return newDigraphToken(IToken.tRBRACKET, start);
				}
				return newToken(IToken.tCOLON, start);

			case '?':
				return newToken(IToken.tQUESTION, start);

			case '+':
				switch (d) {
				case '+':
					nextCharPhase3();
					return newToken(IToken.tINCR, start);
				case '=':
					nextCharPhase3();
					return newToken(IToken.tPLUSASSIGN, start);
				}
				return newToken(IToken.tPLUS, start);

			case '-':
				switch (d) {
				case '>': 
					int e= nextCharPhase3();
					if (e == '*') {
						nextCharPhase3();
						return newToken(IToken.tARROWSTAR, start);
					}
					return newToken(IToken.tARROW, start);

				case '-':
					nextCharPhase3();
					return newToken(IToken.tDECR, start);
				case '=':
					nextCharPhase3();
					return newToken(IToken.tMINUSASSIGN, start);
				}
				return newToken(IToken.tMINUS, start);

			case '*':
				if (d == '=') {
					nextCharPhase3();
					return newToken(IToken.tSTARASSIGN, start);
				}
				return newToken(IToken.tSTAR, start);

			case '/':
				switch (d) {
				case '=':
					nextCharPhase3();
					return newToken(IToken.tDIVASSIGN, start);
				case '/':
					nextCharPhase3();
					lineComment(start);
					popPhase3State();
					continue; 
				case '*':
					blockComment(start, '*');
					popPhase3State();
					continue;
				case '%':
					if (fOptions.fSupportSlashPercentComments) {
						blockComment(start, '%');
						popPhase3State();
						continue;
					}
					break;
				}
				return newToken(IToken.tDIV, start);

			case '%':
				switch (d) {
				case '=':
					nextCharPhase3();
					return newToken(IToken.tMODASSIGN, start);
				case '>':
					nextCharPhase3();
					return newDigraphToken(IToken.tRBRACE, start);
				case ':':
					final int e= nextCharPhase3();
					if (e == '%') {
						markPhase3();
						if (nextCharPhase3() == ':') {
							nextCharPhase3();
							return newDigraphToken(IToken.tPOUNDPOUND, start);
						}
						restorePhase3();
					}
					return newDigraphToken(IToken.tPOUND, start);
				}
				return newToken(IToken.tMOD, start);

			case '^':
				if (d == '=') {
					nextCharPhase3();
					return newToken(IToken.tXORASSIGN, start);
				}
				return newToken(IToken.tXOR, start);

			case '&':
				switch (d) {
				case '&':
					nextCharPhase3();
					return newToken(IToken.tAND, start);
				case '=':
					nextCharPhase3();
					return newToken(IToken.tAMPERASSIGN, start);
				}
				return newToken(IToken.tAMPER, start);

			case '|':
				switch (d) {
				case '|':
					nextCharPhase3();
					return newToken(IToken.tOR, start);
				case '=':
					nextCharPhase3();
					return newToken(IToken.tBITORASSIGN, start);
				}
				return newToken(IToken.tBITOR, start);

			case '~':
				return newToken(IToken.tBITCOMPLEMENT, start);

			case '!':
				if (d == '=') {
					nextCharPhase3();
					return newToken(IToken.tNOTEQUAL, start);
				}
				return newToken(IToken.tNOT, start);

			case '=':
				if (d == '=') {
					nextCharPhase3();
					return newToken(IToken.tEQUAL, start);
				}
				return newToken(IToken.tASSIGN, start);

			case '<':
				if (fInsideIncludeDirective) {
					return headerName(start, false);
				}

				switch(d) {
				case '=':
					nextCharPhase3();
					return newToken(IToken.tLTEQUAL, start);
				case '<':
					final int e= nextCharPhase3();
					if (e == '=') {
						nextCharPhase3();
						return newToken(IToken.tSHIFTLASSIGN, start);
					} 
					return newToken(IToken.tSHIFTL, start);
				case '?':
					if (fOptions.fSupportMinAndMax) {
						nextCharPhase3();
						return newToken(IGCCToken.tMIN, start);
					} 
					break;
				case ':':
					nextCharPhase3();
					return newDigraphToken(IToken.tLBRACKET, start);
				case '%':
					nextCharPhase3();
					return newDigraphToken(IToken.tLBRACE, start);
				}
				return newToken(IToken.tLT, start);

			case '>':
				switch(d) {
				case '=':
					nextCharPhase3();
					return newToken(IToken.tGTEQUAL, start);
				case '>':
					final int e= nextCharPhase3();
					if (e == '=') {
						nextCharPhase3();
						return newToken(IToken.tSHIFTRASSIGN, start);
					} 
					return newToken(IToken.tSHIFTR, start);
				case '?':
					if (fOptions.fSupportMinAndMax) {
						nextCharPhase3();
						return newToken(IGCCToken.tMAX, start);
					} 
					break;
				}
				return newToken(IToken.tGT, start);

			case ',':
				return newToken(IToken.tCOMMA, start);

			default:
				// in case we have some other letter to start an identifier
				if (Character.isUnicodeIdentifierStart((char) c)) {
					return identifier(start, 1);
				}
				break;
			}
			// handles for instance @
			return newToken(tOTHER_CHARACTER, start, 1);
		}
    }

	private Token newToken(int kind, int offset) {
		popPhase3State();
    	return new Token(kind, fSource, offset, fOffset);
    }

	private Token newDigraphToken(int kind, int offset) {
		popPhase3State();
    	return new TokenForDigraph(kind, fSource, offset, fOffset);
    }

    private Token newToken(final int kind, final int offset, final int imageLength) {
    	final int endOffset= fOffset;
    	final int sourceLen= endOffset-offset;
    	char[] image;
    	if (sourceLen != imageLength) {
    		image= getCharImage(offset, endOffset, imageLength);
    	}
    	else {
			image= new char[imageLength];
			fInput.arraycopy(offset, image, 0, imageLength);
    	}
    	popPhase3State();
    	return new TokenWithImage(kind, fSource, offset, endOffset, image);
    }

    private void handleProblem(int problemID, char[] arg, int offset) {
    	fLog.handleProblem(problemID, arg, offset, fOffset);
    }

	private Token headerName(final int start, final boolean expectQuotes) throws OffsetLimitReachedException {
    	int length= 1;
		boolean done = false;
		int c= fCharPhase3;
		loop: while (!done) {
			switch (c) {
			case END_OF_INPUT:
				if (fSupportContentAssist) {
					throw new OffsetLimitReachedException(ORIGIN_LEXER, 
							newToken((expectQuotes ? tQUOTE_HEADER_NAME : tSYSTEM_HEADER_NAME), start, length));
				}
				//$FALL-THROUGH$
			case '\n':
				handleProblem(IProblem.SCANNER_UNBOUNDED_STRING, getInputChars(start, fOffset), start);
				break loop;
				
			case '"':
				done= expectQuotes;
				break;
			case '>':
				done= !expectQuotes;
				break;
			}
			length++;
			c= nextCharPhase3();
		}
		return newToken((expectQuotes ? tQUOTE_HEADER_NAME : tSYSTEM_HEADER_NAME), start, length);
	}

	private void blockComment(final int start, final char trigger) {
		// we can ignore line-splices, trigraphs and windows newlines when searching for the '*'
		int pos= fEndOffset;
		while (isValidOffset(pos)) {
			if (fInput.get(pos++) == trigger) {
				fEndOffset= pos;
				if (nextCharPhase3() == '/') {
					nextCharPhase3();
					fLog.handleComment(true, start, fOffset);
					return;
				}
			}
		}
		fCharPhase3= END_OF_INPUT;
		fOffset= fEndOffset= pos;
		fLog.handleComment(true, start, pos);
	}

	private void lineComment(final int start) {
		int c= fCharPhase3;
		while (true) {
			switch (c) {
			case END_OF_INPUT:
			case '\n':
				fLog.handleComment(false, start, fOffset);
				return;
			}
			c= nextCharPhase3();
		}
	}

	private Token stringLiteral(final int start, int length, final int tokenType) throws OffsetLimitReachedException {
		boolean escaped = false;
		boolean done = false;
		
		int c= fCharPhase3;
		
		loop: while (!done) {
			switch(c) {
			case END_OF_INPUT:
				if (fSupportContentAssist) {
					throw new OffsetLimitReachedException(ORIGIN_LEXER, newToken(tokenType, start, length));
				}
				//$FALL-THROUGH$
			case '\n':
				handleProblem(IProblem.SCANNER_UNBOUNDED_STRING, getInputChars(start, fOffset), start);
				break loop;
				
			case '\\': 
				escaped= !escaped;
				break;
			case '"':
				if (!escaped) {
					done= true;
				}
				escaped= false;
				break;
			default:
				escaped= false;
				break;
			}
			length++;
			c= nextCharPhase3();
		}
		return newToken(tokenType, start, length);
	}

	private Token rawStringLiteral(final int start, int length, final int tokenType) throws OffsetLimitReachedException {
		final int delimOffset= fOffset;
		int delimEndOffset = delimOffset;
		int offset;
		for(;; delimEndOffset++) {
			if (!fInput.isValidOffset(delimEndOffset)) {
				offset= delimEndOffset;
				break;
			}
			if (fInput.get(delimEndOffset) == '(') {
				offset= delimEndOffset+1;
				break;
			}
		}
		
		final int delimLength= delimEndOffset-delimOffset;
		for(;; offset++) {
			if (!fInput.isValidOffset(offset)) {
				handleProblem(IProblem.SCANNER_UNBOUNDED_STRING, getInputChars(start, offset), start);
				break;
			} 
				
			final char c= fInput.get(offset);
			if (c == ')') {
				final int endingDoubleQuoteOffset= offset+delimLength+1;
				if (fInput.isValidOffset(endingDoubleQuoteOffset) && fInput.get(endingDoubleQuoteOffset) == '"') {
					boolean prefixMatches= true;
					for (int i = 0; i < delimLength; i++) {
						if (fInput.get(offset + i + 1) != fInput.get(delimOffset+i)) {
							prefixMatches= false;
							break;
						}
					}
					if (prefixMatches) {
						offset= endingDoubleQuoteOffset+1;
						break;
					}
				}
			}
		}
		fOffset= offset-1;
		fEndOffset= offset;
		fCharPhase3=  0;
		nextCharPhase3();
		return newToken(tokenType, start, offset-start);
	}

	private Token charLiteral(final int start, final int tokenType) throws OffsetLimitReachedException {
		boolean escaped = false;
		boolean done = false;
		int length= tokenType == IToken.tCHAR ? 1 : 2;
		int c= fCharPhase3;
		
		loop: while (!done) {
			switch(c) {
			case END_OF_INPUT:
				if (fSupportContentAssist) {
					throw new OffsetLimitReachedException(ORIGIN_LEXER, newToken(tokenType, start, length));
				}
				//$FALL-THROUGH$
			case '\n':
				handleProblem(IProblem.SCANNER_BAD_CHARACTER, getInputChars(start, fOffset), start);
				break loop;
			case '\\': 
				escaped= !escaped;
				break;
			case '\'':
				if (!escaped) {
					done= true;
				}
				escaped= false;
				break;
			default:
				escaped= false;
				break;
			}
			length++;
			c= nextCharPhase3();
		}
		return newToken(tokenType, start, length);
	}
	
	private Token identifier(int start, int length) {
		int tokenKind= IToken.tIDENTIFIER;
    	boolean isPartOfIdentifier= true;
    	int c= fCharPhase3;
        while (true) {
        	switch(c) {
            case 'a': case 'b': case 'c': case 'd': case 'e': case 'f': case 'g': case 'h': case 'i': 
            case 'j': case 'k': case 'l': case 'm': case 'n': case 'o': case 'p': case 'q': case 'r': 
            case 's': case 't': case 'u': case 'v': case 'w': case 'x': case 'y': case 'z':
            case 'A': case 'B': case 'C': case 'D': case 'E': case 'F': case 'G': case 'H': case 'I':
            case 'J': case 'K': case 'L': case 'M': case 'N': case 'O': case 'P': case 'Q': case 'R': 
            case 'S': case 'T': case 'U': case 'V': case 'W': case 'X': case 'Y': case 'Z':
            case '_': 
            case '0': case '1': case '2': case '3': case '4':
            case '5': case '6': case '7': case '8': case '9':
            	break;
            	
            case '\\': // universal character name
            	markPhase3();
            	switch(nextCharPhase3()) {
            	case 'u': case 'U':
            		length++;
            		break;
            	default:
            		restorePhase3();
            		isPartOfIdentifier= false;
            		break;
            	}
            	break;

            case END_OF_INPUT:
				if (fSupportContentAssist) {
					tokenKind= IToken.tCOMPLETION;
				}
				isPartOfIdentifier= false;
				break;
            case ' ': case '\t': case 0xb: case '\f': case '\r': case '\n':
                isPartOfIdentifier= false;
            	break;

            case '$':
            	isPartOfIdentifier= fOptions.fSupportDollarInIdentifiers;
            	break;
            case '@':
            	isPartOfIdentifier= fOptions.fSupportAtSignInIdentifiers;
            	break;
            	
            case '{': case '}': case '[': case ']': case '#': case '(': case ')': case '<': case '>':
            case '%': case ':': case ';': case '.': case '?': case '*': case '+': case '-': case '/':
            case '^': case '&': case '|': case '~': case '!': case '=': case ',': case '"': case '\'':
            	isPartOfIdentifier= false;
            	break;
            	
            default:
            	isPartOfIdentifier= Character.isUnicodeIdentifierPart((char) c);
            	break;
        	}
        	
        	if (!isPartOfIdentifier) {
        		break;
        	}
        	
        	length++;
        	c= nextCharPhase3();
        }

        return newToken(tokenKind, start, length);
	}
	
	private Token number() throws OffsetLimitReachedException {
		NumberToken nt = fNumberParser.getNumber();

		return newToken(nt.getKind(), nt.getOffset(), nt.getLength());
	}
	
	// Checks the next token will be an identifier
	private boolean isIdentifierStart() {
		final int c = fCharPhase3;
		markPhase3();
		int d = nextCharPhase3();
		boolean ret = false;

		lookup: switch (c) {
		case 'L':
			switch (d) {
			case 'R':
				markPhase3();
				if (nextCharPhase3() == '"') {
					break lookup;
				}
				restorePhase3();
				break;
			case '"':
			case '\'':
				break lookup;
			}
			ret = true;
			break lookup;
		case 'u':
		case 'U':
			if (fOptions.fSupportUTFLiterals) {
				switch (d) {
				case 'R':
					markPhase3();
					if (nextCharPhase3() == '"') {
						break lookup;
					}
					restorePhase3();
					break;
				case '"':
				case '\'':
					break lookup;
				case '8':
					if (c == 'u') {
						markPhase3();
						switch (nextCharPhase3()) {
						case 'R':
							if (nextCharPhase3() == '"') {
								break lookup;
							}
							break;
						case '"':
							break lookup;
						}
						restorePhase3();
					}
					break;
				}
			}
			ret = true;
			break lookup;

		case 'R':
			if (d == '"') {
				break lookup;
			}
			ret =  true;
			break lookup;
		case '"':
		case '\'':
			break lookup;

		case 'a': case 'b': case 'c': case 'd': case 'e': case 'f': case 'g': case 'h': case 'i': 
		case 'j': case 'k': case 'l': case 'm': case 'n': case 'o': case 'p': case 'q': case 'r': 
		case 's': case 't':           case 'v': case 'w': case 'x': case 'y': case 'z':
		case 'A': case 'B': case 'C': case 'D': case 'E': case 'F': case 'G': case 'H': case 'I':
		case 'J': case 'K':           case 'M': case 'N': case 'O': case 'P': case 'Q':  
		case 'S': case 'T':           case 'V': case 'W': case 'X': case 'Y': case 'Z':
		case '_':
			ret = true;
			break lookup;

		case '$':
			ret = fOptions.fSupportDollarInIdentifiers;
			break lookup;
		case '@':
			ret = fOptions.fSupportAtSignInIdentifiers;
			break lookup;
		default:
			if (Character.isUnicodeIdentifierStart((char) c)) {
				ret = true;
			}
			break;
		}
		restorePhase3();
		return ret;
	}

	
	/**
	 * Saves the current state of phase3, necessary for '...', '%:%:', UNCs and string literals
	 * with a long prefix.
	 */
	private void markPhase3() {
		fMarkOffset.addFirst(fOffset);
		fMarkEndOffset.addFirst(fEndOffset);
		fMarkPrefetchedChar.addFirst(fCharPhase3);
		fDequeDepth++;
	}
	
	/**
	 * Restores a previously saved state of phase3.
	 */
	private void restorePhase3() {
		fOffset= fMarkOffset.removeFirst();
		fEndOffset= fMarkEndOffset.removeFirst();
		fCharPhase3= fMarkPrefetchedChar.removeFirst();
		fDequeDepth--;
	}
	
	private void popPhase3State() {
		if (fDequeDepth > 0) {
			fMarkOffset.removeFirst();
			fMarkEndOffset.removeFirst();
			fMarkPrefetchedChar.removeFirst();
			fDequeDepth--;
		}
	}
	
	/**
	 * Perform phase 1-3: Replace \r\n with \n, handle trigraphs, detect line-splicing.
	 * Changes fOffset, fEndOffset and fCharPhase3, state-less otherwise.
	 */
	private int nextCharPhase3() {
		int pos= fEndOffset;
		do {
			if (!isValidOffset(pos+1)) {
				if (!isValidOffset(pos)) {
					fOffset= pos;
					fEndOffset= pos;
					fCharPhase3= END_OF_INPUT;
					return END_OF_INPUT;
				}
				fOffset= pos;
				fEndOffset= pos+1;
				fCharPhase3= fInput.get(pos);
				return fCharPhase3;
			}
			
			final char c= fInput.get(pos);
			fOffset= pos;
			fEndOffset= ++pos;
			fCharPhase3= c;
			switch(c) {
			// windows line-ending
			case '\r':
				if (fInput.get(pos) == '\n') {	
					fEndOffset= pos+1;
					fCharPhase3= '\n';
					return '\n';
				}
				return c;

				// trigraph sequences
			case '?':
				if (fInput.get(pos) != '?' || !isValidOffset(pos+1)) {
					return c;
				}
				final char trigraph= checkTrigraph(fInput.get(pos+1));
				if (trigraph == 0) {
					return c;
				}
				if (trigraph != '\\') {
					fEndOffset= pos+2;
					fCharPhase3= trigraph;
					return trigraph;
				}
				pos+= 2;
				// $FALL-THROUGH$, handle backslash

			case '\\':
				final int lsPos= findEndOfLineSpliceSequence(pos);
				if (lsPos > pos) {
					pos= lsPos;
					continue;
				}
				fEndOffset= pos;
				fCharPhase3= '\\';
				return '\\';	// don't return c, it may be a '?'

			default:
				return c;
			}
		}
		while (true);
	}
	
	/**
	 * Maps a trigraph to the character it encodes.
	 * @param c trigraph without leading question marks.
	 * @return the character encoded or 0.
	 */
	private char checkTrigraph(char c) {
		switch(c) {
		case '=': return '#';
		case '\'':return '^';
		case '(': return '[';
		case ')': return ']';
		case '!': return '|';
		case '<': return '{';
		case '>': return '}';
		case '-': return '~';
		case '/': return '\\';
		}
		return 0;
	}

	/**
	 * Returns the end-offset for a line-splice sequence, or -1 if there is none.
	 */
	private int findEndOfLineSpliceSequence(int pos) {
		boolean haveBackslash= true;
		int result= -1;
		loop: while (isValidOffset(pos)) {
			switch(fInput.get(pos++)) {
			case '\n':	
				if (haveBackslash) {
					result= pos;
					haveBackslash= false;
					continue loop;
				}
				return result; 					
		
			case '\r': case ' ': case '\f': case '\t': case 0xb: // vertical tab  
				if (haveBackslash) {
					continue loop;
				}
				return result;
			
			case '?':
				if (!isValidOffset(pos+1) || fInput.get(pos) != '?' || fInput.get(++pos) != '/') {
					return result;
				}
				// $FALL-THROUGH$ to backslash handling
					
			case '\\':
				if (!haveBackslash) {
					haveBackslash= true;
					continue loop;
				}
				return result;

			default:
				return result;
			}
		}
		return result;
	}

	/**
	 * Returns the image from the input without any modification.
	 */
	public char[] getInputChars(int offset, int endOffset) {
		final int length= endOffset-offset;
		if (length <= 0) {
			return CharArrayUtils.EMPTY;
		}
		final char[] result= new char[length];
		fInput.arraycopy(offset, result, 0, length);
		return result;
	}

	AbstractCharArray getInput() {
		return fInput;
	}
	
	/**
	 * Returns the image with trigraphs replaced and line-splices removed.
	 */
	private char[] getCharImage(int offset, int endOffset, int imageLength) {
		final char[] result= new char[imageLength];
		markPhase3();
		fEndOffset= offset;
		for (int idx=0; idx<imageLength; idx++) {
			result[idx]= (char) nextCharPhase3();
		}
		restorePhase3();
		return result;
	}

	public void resetStateStack() {
		while (fDequeDepth-- > 0) {
			fMarkOffset.removeFirst();
			fMarkEndOffset.removeFirst();
			fMarkPrefetchedChar.removeFirst();
		}
	}

	public void saveState() {
		fMarkOffset.addFirst(fOffset);
		fMarkEndOffset.addFirst(fEndOffset);
		fMarkPrefetchedChar.addFirst(fCharPhase3);
		fMarkInsideIncludeDirective= fInsideIncludeDirective;
		fMarkToken= fToken;
		fMarkLastToken= fLastToken;
		fDequeDepth++;
	}

	public void restoreState() {
		fOffset= fMarkOffset.removeFirst();
		fEndOffset= fMarkEndOffset.removeFirst();
		fCharPhase3= fMarkPrefetchedChar.removeFirst();
		fInsideIncludeDirective= fMarkInsideIncludeDirective;
		fToken= fMarkToken;
		fLastToken= fMarkLastToken;
		fDequeDepth--;
	}
}
