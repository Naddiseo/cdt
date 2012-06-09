/*******************************************************************************
 * Copyright (c) 2012 Richard Eames
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     Richard Eames - Initial implementation
 *******************************************************************************/
package org.eclipse.cdt.internal.core.parser.scanner;

import org.eclipse.cdt.core.parser.IToken;
import org.eclipse.cdt.core.parser.util.CharArrayUtils;
import org.eclipse.cdt.internal.core.parser.scanner.Lexer.LexerOptions;
import org.eclipse.cdt.internal.core.parser.scanner.NumberToken.NumberType;

/**
 * 
 * Given an interface to a CharArray, finds a
 *
 */
public class NumberParser {
	
	public interface CharGetter {
		public int get(); // Returns the current character
		public int next(); // Advances to the next character, and returns it
		public Token getIdentifier(); // A way of parsing an identifier
		public void mark(); // Save the parser state
		public void restore(); // Restore the parser state
		public int getOffset(); // Offset into the CharArray
		public char[] getSubstring(int start, int length);
		public boolean isIdentifierStart(); // is the current character the start of an identifier
	}
	
	private int fStart; // Start of the number into the CharArray
	private int fChar; // Current char
	
	private final LexerOptions fOptions;
	private final char[] fAdditionalNumericLiteralSuffixes;
	private final CharGetter fGetter;
	
	public NumberParser(LexerOptions options, char[] additionalValidSuffixes, CharGetter getter) {
		fOptions = options;
		fAdditionalNumericLiteralSuffixes = additionalValidSuffixes;
		fGetter = getter;

		fStart = getOffset();
		fChar = fGetter.get();
	}
	
	public NumberToken getNumber() {
		fStart = getOffset();
		fChar = fGetter.get();
		if (fChar == '.') {
			return afterDecimalPoint();
		}
		else {
			return integerLiteral();
		}
	}
	
	// Ref: 2.14.4
	//private NumberToken floatLiteral() {
		
	//}

	// Ref: 2.14.2
	private NumberToken integerLiteral() {
		NumberType tp = NumberType.DECIMAL;
		boolean failed = false;
		
		if (fChar == '0') {
			// Probably octal/hex/binary
			next();
			
			if ((fChar | 0x20) == 'x') {
				return probablyHex();
			}
			else if ((fChar | 0x20) == 'b') {
				return probablyBinary();
			}
			else if (isOctalDigit()) {
				/* octal-literal:
				 *   0
				 *   octal-literal octal-digit
				 */
				while (isOctalDigit()) {
					next();
				}
				tp = NumberType.OCTAL;
				
			}
			else if (fChar == '.') {
				return afterDecimalPoint();
			}
			/*
			 * At this point, if we have 8 or 9, then
			 * we have a malformed octal  
			 */
			if (fChar == '8' || fChar == '9') {
				tp = NumberType.OCTAL;
				failed = true;
			}
		}
		else if (Character.isDigit((char)fChar)) {
			/* decimal-literal :
			 *    nonzero-digit         (c has to be this to get into this else)
			 *    decimal-literal digit
			 */
			while (Character.isDigit((char)fChar)) {
				next();
			}
			
			if (fChar == '.') {
				// Looks like it was a float
				return afterDecimalPoint();
			}
			else if ((fChar | 0x20) == 'e') {
				return exponentPart();
			}
		}
		else {
			// Somehow we got called and there wasn't a digit
			// Shouldn't get here
			failed = true;
		}
		
		return nextToken(IToken.tINTEGER, tp, failed);
	}
	
	/*
	 * Called with the exptectation that lower(fChar) == '.
	 */
	private NumberToken afterDecimalPoint() {
		int c = next();
		while (Character.isDigit((char)c)) {
			c = next();
		}
		
		if ((c | 0x20) == 'e') {
			return exponentPart();
		}
		
		/*
		 * This function can't "fail" because 1. is a valid float 
		 */
		return nextToken(IToken.tFLOATINGPT, NumberType.FLOAT, false);
	}
	
	/*
	 * Called with the expectation that lower(fChar) == 'e'
	 */
	private NumberToken exponentPart() {
		int c = next();
		
		// optional '+' or '-'
		if (c == '+' || c == '-') {
			c = next();
		}
		while (Character.isDigit((char)c)) {
			c = next();
		}
		// If there were no digits following the 'e' then we have
		// D.De or .De which is a UDL on a double
		
		return nextToken(IToken.tFLOATINGPT, NumberType.FLOAT, false);
	}
	
	// GCC's binary constant notation
	private NumberToken probablyBinary() {
		NumberType tp = NumberType.DECIMAL;
		boolean failed = false;
		
		mark();
		int c = next();
		if (c == '1' || c == '0') {
			while (c == '1' || c == '0') {
				c = next();
			}
			tp = NumberType.BINARY;
			if (Character.isDigit((char)c) || !isIdentifierStart()) {
				// UDL can't begin with a digit, so this is a malformed binary
				
				if (c == '.') {
					// No such thing as binary floating point (yet)
					c = next();
					while (Character.isDigit((char)c)) {
						c = next();
					}
				}
				else {
					next(); // TODO: is this safe consuming the bad number?
				}
				failed = true;
			}
		}
		else {
			// Here we have 0b or 0B which could be a UDL
			restore();
		}
		
		return nextToken(IToken.tINTEGER, tp, failed);
	}
	
	private NumberToken probablyHex() {
		/* hexadecimal-literal
		 *   0x hexadecimal-digit
		 *   0X hexadecimal-digit
		 *   hexadecimal-literal hexadecimal-digit
		 */
		NumberType tp = NumberType.DECIMAL;
		mark();
		next();
		if (isHexDigit()) {
			while (isHexDigit()) {
				next();
			}
			tp = NumberType.HEX;
			if (fChar == '.') {
				// Could be GCC's Hex Float
				return probablyHexFloat();
			}
		}
		else {
			// Here we have 0x or 0X which could be a UDL
			restore();
		}
		
		return nextToken(IToken.tINTEGER, tp, false);
	}
	
	private NumberToken probablyHexFloat() {
		// 0xHHH.HHH[pP]DDDD
		NumberType tp = NumberType.HEX;
		boolean failed = false;
		mark();
		next();
		if (isHexDigit()) {
			while (isHexDigit()) {
				next();
			}
			
			if ((fChar | 0x20) == 'p') {
				mark();
				next();
				
				if (fChar == '-' || fChar == '+') {
					next();
				}
				
				if (Character.isDigit((char)fChar)) {
					while (Character.isDigit((char)fChar)) {
						next();
					}
					tp = NumberType.HEXFLOAT;
				}
				else {
					restore();
					failed = true;
				}
			}
			else {
				// The parser can get very confused at this
				// point as the expression can be 0x1.f
				restore();
				failed = true;
			}
		}
		else {
			// Here we have 0xHHH.
			restore();
			failed = true;
		}
		
		return nextToken(IToken.tINTEGER, tp, failed);
	}
	
	private boolean isHexDigit() {
		int c =  fChar | 0x20;
		return ((c <= 'f' && c >= 'a') || (c <= '9' && c >= '0'));
	}
	
	private boolean isOctalDigit() {
		return fChar >= '0' && fChar <= '7';
	}
	
	private boolean isIdentifierStart() {
		return fGetter.isIdentifierStart();
	}
	
	private Token getSuffix() {
		if (isIdentifierStart()) {
			return fGetter.getIdentifier();
		}
		return null;
	}
	
	private boolean isCompilerSuffix(char[] suffix, int kind) {
		boolean result = true;
		
		if (kind == IToken.tINTEGER) {
			loop: for (char c : suffix) {
				switch (c) {
				case 'u': case 'U':
				case 'l': case 'L':
					continue loop;
				
				default:
					result = false;
				}
				boolean found = false;
				for (char d : fAdditionalNumericLiteralSuffixes) {
					if (d == c) {
						found = true;
					}
				}
				if (!found && !result) {
					return false;
				}
			}
		}
		else if (kind == IToken.tFLOATINGPT) {
			loop: for (char c : suffix) {
				switch (c) {
				case 'u': case 'U':
				case 'l': case 'L':
				case 'f': case 'F':
					continue loop;
				
				default:
					result = false;
				}
				boolean found = false;
				for (char d : fAdditionalNumericLiteralSuffixes) {
					if (d == c) {
						found = true;
					}
				}
				if (!found && !result) {
					return false;
				}
			}
		}
		
		return result;
	}
	
	private int getLength() {
		return getOffset() - fStart;
	}
	
	private NumberToken nextToken(int kind, NumberType type, boolean failed) {
		Token udSuffix = getSuffix();
		int suffixOffset = getLength();
		char[] result = CharArrayUtils.EMPTY;
		
		result = fGetter.getSubstring(fStart, getLength());
		
		if (udSuffix != null) {
			suffixOffset = udSuffix.getOffset();
			if (!isCompilerSuffix(udSuffix.getCharImage(), kind)) {
				if (fOptions.fSupportUserDefinedLiterals) {
					if (kind == IToken.tINTEGER) {
						kind = IToken.tUSER_DEFINED_INTEGER_LITERAL;
					}
					else if (kind == IToken.tFLOATINGPT) {
						kind = IToken.tUSER_DEFINED_FLOATING_LITERAL;
					}
				}
				else {
					failed =  true;
				}
			}
		}		
		return new NumberToken(result, type, kind, suffixOffset, fStart, failed);
	}
	
	private int next() {
		fChar = fGetter.next();
		return fChar;
	}
	
	private void mark() {
		fGetter.mark();
	}
	
	private void restore() {
		fGetter.restore();
	}
	
	private int getOffset() {
		return fGetter.getOffset();
	}
}