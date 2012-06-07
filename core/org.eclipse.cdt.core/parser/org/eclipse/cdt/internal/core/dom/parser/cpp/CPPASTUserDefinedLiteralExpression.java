package org.eclipse.cdt.internal.core.dom.parser.cpp;

import org.eclipse.cdt.core.parser.util.CharArrayUtils;

public class CPPASTUserDefinedLiteralExpression extends CPPASTLiteralExpression {
	private char[] suffix = CharArrayUtils.EMPTY;
	
	public CPPASTUserDefinedLiteralExpression() { }
	
	public CPPASTUserDefinedLiteralExpression(int kind, char[] value) {
		super(kind, value);
		this.suffix = extractSuffixFromValue();
	}
	
	public CPPASTUserDefinedLiteralExpression(int kind, char[] value, char[] suffix) {
		super(kind, value);
		this.suffix = suffix;
	}
	
	@Override
	public CPPASTUserDefinedLiteralExpression copy(CopyStyle style) {
		CPPASTUserDefinedLiteralExpression copy = new CPPASTUserDefinedLiteralExpression(
			getKind(),
			getValue() == null ? null : getValue().clone(),
			suffix == null ? null : suffix.clone()
		);
		copy.setOffsetAndLength(this);
		if (style == CopyStyle.withLocations) {
			copy.setCopyLocation(this);
		}
		return copy;
	}
	
	public char[] getSuffix() {
		return suffix;
	}
	
	public void setSuffix(char[] value) {
		assertNotFrozen();
		this.suffix = value;
	}
	
	/* 
	 * Parse valid integer and float literals, anything after the valid part
	 * must be the suffix, whether that's user-defined or not.
	 * 
	 * refs: 2.14.2, 2.14.4 
	 */
	private char[] extractSuffixFromValue() {
		final char[] lit = getValue();
		int i = 0;
		if (lit.length <= 0) {
			return CharArrayUtils.EMPTY;
		}
		char c = lit[i];
		
		try {
			
			if (c == '.') {
				// parse float
				i = parseFloat(i);
			}
			else {
				// int-or-float
				
				if (c == '0') {
					// probably octal or hex
					c = lit[++i];
					
					if ((c | 0x20) == 'x') {
						// Hex
						do {
							c = lit[++i];
						} while (isHexDigit(c));
						
					}
					else if (isOctalDigit(c)) {
						// Octal
						do {
							c = lit[++i];
						} while (isOctalDigit(c));
					}
					else if (c == '.') {
						// need to include the '0' in case it's "0."
						i = parseFloat(i-1); 
					}
				}
				else {
					int startNum = i;
					do {
						c = lit[++i];
					} while (isNonZeroDigit(c));
					
					if (c == '.' || ((c | 0x20) == 'e')) {
						i = parseFloat(startNum);
					}
				}
				
			}
		} 
		catch (ArrayIndexOutOfBoundsException e) {
			/*
			 *  safe to ignore this, it just means there is no suffix
			 *  or the literal is empty
			 */
		}
		catch (Exception e) {
			e.printStackTrace();
		}
		
		// At this point `i` should be the offset of the suffix
		return toString().substring(i).toCharArray();
	}
	
	private int parseFloat(int offset) {
		final char[] lit = getValue();
		boolean seenDecimalPoint = false;
		
		if (lit[offset] == '.') {
			offset++;
			seenDecimalPoint = true;
		}
		
		do {
			offset++;
		} while (Character.isDigit(lit[offset]));
		
		if (!seenDecimalPoint && lit[offset] == '.') {
			offset++;
			seenDecimalPoint = true;
		}
		
		while (Character.isDigit(lit[offset])) {
			offset++;
		}
		
		// either 'e' | 'E' | suffix
		
		if ((lit[offset] | 0x20) == 'e') {
			offset++;
			// Optional 'sign'
			if (lit[offset] == '-' || lit[offset] == '+') {
				offset++;
			}
			do {
				offset++;
			} while (Character.isDigit(lit[offset]));
		}
		
		return offset;
	}
	
	private boolean isNonZeroDigit(char c) {
		return (c <= '9' && c >= '1');
	}
	
	private boolean isOctalDigit(char c) {
		return (c <= '7' && c >= '0');
	}
	
	private boolean isHexDigit(char c) {
		return isNonZeroDigit(c) || c == '0' || ((c | 0x20) <= 'f' && (c | 0x20) >= 'a');
	}
}
