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
	private Deque<Integer> fMarkOffset;
	
	public DefaultCharGetter(char[] image, int start) {
		this(new CharArray(image), start);
	}
	
	DefaultCharGetter(AbstractCharArray image, int start) {
		fImage = image;
		fOffset = start;
		fMarkOffset = new ArrayDeque<Integer>();
	}

	@Override
	public int get() {
		return fImage.get(fOffset);
	}

	@Override
	public int next() {
		if (fImage.isValidOffset(fOffset + 1)) {
			return fImage.get(++fOffset);
		}
		return -1;
	}

	@Override
	public Token getIdentifier() {
		// Assume that the end of the number to the end of the string
		// is an identifier
		return new TokenWithImage(IToken.tIDENTIFIER, this, fOffset, fImage.getLength(), fImage.toString().toCharArray());
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

}
