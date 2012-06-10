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

/**
 * A token returned from NumberParser
 * Assumes the number was extracted from a CharArray, but also contains its own
 * copy
 */
public class NumberToken {
	public enum NumberType {
		DECIMAL,
		HEX,
		OCTAL,
		BINARY,
		FLOAT,
		HEXFLOAT,
	}
	
	private final AbstractCharArray fImage; // It's own copy of the number + suffix
	private final int fKind; // The type of IToken this is.
	private final int fSuffixOffset; // the offset into fImage where the suffix begins
	private final int fOffset; // the offset into the original CharArray
	private final NumberType fType; // The type of number literal
	private final boolean fFailed; // Whether the parse failed
	
	public NumberToken(char[] image, NumberType type, int kind, int suffixOffset, int offset, boolean failed) {
		this(new CharArray(image), type, kind, suffixOffset, offset, failed);
	}
	
	public NumberToken(AbstractCharArray image, NumberType type, int kind, int suffixOffset, int offset, boolean failed) {
		this.fImage = image;
		this.fType = type;
		this.fKind = kind;
		this.fSuffixOffset = suffixOffset;
		this.fOffset = offset;
		this.fFailed = failed;
	}
	
	public String getImage() {
		return fImage.toString();
	}
	
	public char[] getCharImage() {
		return getImage().toCharArray();
	}
	
	public char[] getNumber() {
		final char[] result = new char[fSuffixOffset];
		fImage.arraycopy(0, result, 0, fSuffixOffset);
		return result;
	}
	
	public char[] getSuffix() {
		final char[] result = new char[fImage.getLength() - fSuffixOffset];
		fImage.arraycopy(fSuffixOffset, result, 0, fImage.getLength() - fSuffixOffset);
		return result;
	}
	
	public int getKind() {
		return fKind;
	}
	
	public int getOffset() {
		return fOffset;
	}
	
	public int getEndOffset() {
		return getOffset() + getLength();
	}
	
	public int getLength() {
		return fImage.getLength();
	}
	
	public NumberType getType() {
		return fType;
	}
	
	public boolean hasFailed() {
		return fFailed;
	}
}
