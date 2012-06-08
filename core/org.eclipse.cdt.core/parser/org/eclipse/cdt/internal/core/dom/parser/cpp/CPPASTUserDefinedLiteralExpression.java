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
package org.eclipse.cdt.internal.core.dom.parser.cpp;

import org.eclipse.cdt.core.parser.util.CharArrayUtils;

public class CPPASTUserDefinedLiteralExpression extends CPPASTLiteralExpression {
	private char[] suffix = CharArrayUtils.EMPTY;
	
	public CPPASTUserDefinedLiteralExpression() { }
	
	public CPPASTUserDefinedLiteralExpression(int kind, char[] value/*, char[] suffix*/) {
		super(kind, value);
		//this.suffix = suffix;
	}
	
	@Override
	public CPPASTUserDefinedLiteralExpression copy(CopyStyle style) {
		CPPASTUserDefinedLiteralExpression copy = new CPPASTUserDefinedLiteralExpression(
			getKind(),
			getValue() == null ? null : getValue().clone()/*,
			suffix == null ? null : suffix.clone()*/
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
}
