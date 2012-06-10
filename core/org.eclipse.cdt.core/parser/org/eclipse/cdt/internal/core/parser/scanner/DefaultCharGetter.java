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

import java.util.ArrayDeque;
import java.util.Deque;

import org.eclipse.cdt.core.parser.IToken;
import org.eclipse.cdt.internal.core.parser.scanner.NumberParser.CharGetter;

public class DefaultCharGetter implements CharGetter {
	
	private AbstractCharArray fImage;
	private int fOffset;
	private int fChar;
	private Deque<Integer> fMarkOffset;
	
	public DefaultCharGetter(char[] image, int start) {
		this(new CharArray(image), start);
	}
	
	DefaultCharGetter(AbstractCharArray image, int start) {
		fImage = image;
		fOffset = start;
		fChar = (fImage.tryGetLength() > 0) ? fImage.get(0) : -1;
		fMarkOffset = new ArrayDeque<Integer>();
	}

	@Override
	public int get() {
		return fChar;
	}

	@Override
	public int next() {
		if (fImage.isValidOffset(fOffset + 1)) {
			fChar = fImage.get(++fOffset);
		}
		else {
			fChar = -1;
		}
		return fChar;
	}

	@Override
	public Token getIdentifier() {
		// Assume that the end of the number to the end of the string
		// is an identifier
		char[] image = new char[fImage.getLength() - fOffset];
		fImage.arraycopy(fOffset, image, 0, image.length);
		return new TokenWithImage(IToken.tIDENTIFIER, this, fOffset, fImage.getLength(), image);
	}

	@Override
	public void mark() {
		fMarkOffset.push(fOffset);
	}

	@Override
	public void restore() {
		fOffset = fMarkOffset.pop();
	}

	@Override
	public int getOffset() {
		return fOffset;
	}

	@Override
	public char[] getSubstring(int start, int length) {
		char [] result = new char[length];
		fImage.arraycopy(start, result, 0, length);
		return result;
	}

	@Override
	public boolean isIdentifierStart() {
		int c = get();
		return Character.isLetter(c) || c == '_'; // Naive check
	}

	@Override
	public char[] getCharImage(int offset, int endOffset, int imageLength) {
		char[] result = new char[imageLength];
		int j = 0;
		for (int i = offset; i < endOffset; i++) {
			char c = fImage.get(i);
			if (c != '\n' && c != '\r') {
				result[j++] = c;
			}
		}
		return result;
	}

}
