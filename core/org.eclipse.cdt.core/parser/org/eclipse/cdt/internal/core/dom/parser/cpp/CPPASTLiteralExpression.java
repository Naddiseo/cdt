/*******************************************************************************
 * Copyright (c) 2004, 2011 IBM Corporation and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 * 
 * Contributors:
 *     John Camelon (IBM) - Initial API and implementation
 *     Markus Schorn (Wind River Systems)
 *     Richard
 *******************************************************************************/
package org.eclipse.cdt.internal.core.dom.parser.cpp;

import org.eclipse.cdt.core.dom.ast.ASTVisitor;
import org.eclipse.cdt.core.dom.ast.DOMException;
import org.eclipse.cdt.core.dom.ast.IASTLiteralExpression;
import org.eclipse.cdt.core.dom.ast.IBasicType;
import org.eclipse.cdt.core.dom.ast.IBasicType.Kind;
import org.eclipse.cdt.core.dom.ast.IBinding;
import org.eclipse.cdt.core.dom.ast.IScope;
import org.eclipse.cdt.core.dom.ast.ISemanticProblem;
import org.eclipse.cdt.core.dom.ast.IType;
import org.eclipse.cdt.core.dom.ast.IValue;
import org.eclipse.cdt.core.dom.ast.cpp.ICPPASTLiteralExpression;
import org.eclipse.cdt.core.dom.ast.cpp.ICPPFunction;
import org.eclipse.cdt.core.parser.util.CharArrayUtils;
import org.eclipse.cdt.internal.core.dom.parser.ASTNode;
import org.eclipse.cdt.internal.core.dom.parser.ProblemType;
import org.eclipse.cdt.internal.core.dom.parser.Value;
import org.eclipse.cdt.internal.core.dom.parser.cpp.semantics.CPPSemantics;
import org.eclipse.cdt.internal.core.dom.parser.cpp.semantics.CPPVisitor;

/**
 * Represents a C++ literal.
 */
public class CPPASTLiteralExpression extends ASTNode implements ICPPASTLiteralExpression {
	public static final CPPASTLiteralExpression INT_ZERO =
			new CPPASTLiteralExpression(lk_integer_constant, new char[] {'0'});
	
    private int kind;
    private char[] value = CharArrayUtils.EMPTY;
    private char[] suffix = CharArrayUtils.EMPTY;
    private boolean isCompilerSuffix = true;

    public CPPASTLiteralExpression() {
	}

	public CPPASTLiteralExpression(int kind, char[] value) {
		this.kind = kind;
		this.value = value;
	}
	
	public CPPASTLiteralExpression(int kind, char[] value, char[] suffix) {
		this(kind, value);
		this.setSuffix(suffix);
	}

	@Override
	public CPPASTLiteralExpression copy() {
		return copy(CopyStyle.withoutLocations);
	}
	
	@Override
	public CPPASTLiteralExpression copy(CopyStyle style) {
		CPPASTLiteralExpression copy = new CPPASTLiteralExpression(kind,
				value == null ? null : value.clone(),
				getSuffix() == null ? null : getSuffix().clone());
		copy.setOffsetAndLength(this);
		if (style == CopyStyle.withLocations) {
			copy.setCopyLocation(this);
		}
		return copy;
	}

	@Override
	public int getKind() {
        return kind;
    }

    @Override
	public void setKind(int value) {
        assertNotFrozen();
        kind = value;
    }

    @Override
	public char[] getValue() {
    	return value;
    }

    @Override
	public void setValue(char[] value) {
        assertNotFrozen();
    	this.value= value;
    }
    
    public char[] getSuffix() {
		return suffix;
	}

	public void setSuffix(char[] suffix) {
		this.suffix = suffix;
	}
	
	public void calculateSuffix(char[] compilerSuffixes) {
		try {
			int udOffset = (value[0] == '.' ? afterDecimalPoint(0) : integerLiteral());
			if (udOffset > 0) {
				/*
				 * 2.14.8.1
				 * "If a token matches both user-defined-literal and another literal kind, it is treated as the latter"
				 */
				setSuffix(CharArrayUtils.subarray(value, udOffset, -1));
				for (int i = 0; i < suffix.length; i++) {
					switch (suffix[i]) {
					case 'l': case 'L': 
					case 'u': case 'U':
					case 'f': case 'F':
						continue;
					}
					for (int j = 0; j < compilerSuffixes.length; j++) {
						if (suffix[i] == compilerSuffixes[j]) {
							continue;
						}
					}
					isCompilerSuffix = false;
					// Remove the suffix from the value if it's a UDL
					setValue(CharArrayUtils.subarray(value, 0, udOffset));
					break;
				}
				
			}
		}
		catch (ArrayIndexOutOfBoundsException e) {
			// pass
		}
	}
	@Override
	public String toString() {
        return new String(value);
    }

    @Override
	public boolean accept(ASTVisitor action) {
        if (action.shouldVisitExpressions) {
		    switch (action.visit(this)) {
	            case ASTVisitor.PROCESS_ABORT: return false;
	            case ASTVisitor.PROCESS_SKIP: return true;
	            default: break;
	        }
		}
        if (action.shouldVisitExpressions) {
		    switch (action.leave(this)) {
	            case ASTVisitor.PROCESS_ABORT: return false;
	            case ASTVisitor.PROCESS_SKIP: return true;
	            default: break;
	        }
		}  
        return true;
    }
    
    @Override
	public IType getExpressionType() {
    	switch (getKind()) {
    		case lk_this: {
    			IScope scope = CPPVisitor.getContainingScope(this);
    			IType type= CPPVisitor.getImpliedObjectType(scope);
    			if (type == null) {
    				return new ProblemType(ISemanticProblem.TYPE_UNRESOLVED_NAME);
    			}
    			return new CPPPointerType(type);
    		}
    		case lk_true:
    		case lk_false:
    			return CPPBasicType.BOOLEAN;
    		case lk_char_constant:
    			return getCharType();
    		case lk_float_constant: 
    			return classifyTypeOfFloatLiteral();
    		case lk_integer_constant: 
    			return classifyTypeOfIntLiteral();
    		case lk_string_literal:
    			return getStringType();
    		case lk_nullptr:
    			return CPPBasicType.NULL_PTR;
    	}
		return new ProblemType(ISemanticProblem.TYPE_UNKNOWN_FOR_EXPRESSION);
    }
    
	@Override
	public boolean isLValue() {
		return getKind() == IASTLiteralExpression.lk_string_literal;
	}
	
	@Override
	public ValueCategory getValueCategory() {
		return isLValue() ? ValueCategory.LVALUE : ValueCategory.PRVALUE;
	}
	
	@SuppressWarnings("nls")
	public char[] getOperatorName() {
		return CharArrayUtils.concat("operator\"\"".toCharArray(), suffix);
	}
	
	// 13.5.8
	private IType getUDLOperatorType() {
		IType ret = new ProblemType(ISemanticProblem.TYPE_UNRESOLVED_NAME);
		
		try {
			IBinding func = CPPSemantics.findUDLOperator(this);
			if (func != null) {
				if (func instanceof ICPPFunction) {
					ret = ((ICPPFunction) func).getType().getReturnType();
				}
			}
		} catch (DOMException e) { /* pass, return problem type */ }
		
		return ret;
	}
	
	private IValue getStringLiteralSize() {
		char[] value= getValue();
		int length= value.length-1;
		boolean isRaw= false;
		for (int i = 0; i < length; i++) {
			final char c = value[i];
			if (c == '"') {
				if (isRaw) {
					for (int j = i + 1; j < length; j++) {
						final char d= value[j];
						if (d == '(') {
							length -= 2*(j-i);
							break;
						}
					}
				}
				length -= i;
				if (length < 0)
					length = 0;
				break;
			} else if (c == 'R') {
				isRaw = true;
			}
		}
		return Value.create(length);
	}
	
	private IType getStringType() {
		if (suffix.length > 0) {
			return getUDLOperatorType();
		}
		
		IType type = new CPPBasicType(getBasicCharKind(), 0, this);
		type = new CPPQualifierType(type, true, false);
		return new CPPArrayType(type, getStringLiteralSize());
	}
	
	private IType getCharType() {
		return (suffix.length > 0 ? getUDLOperatorType() : new CPPBasicType(getBasicCharKind(), 0, this));
    }
	
	public Kind getBasicCharKind() {
		switch (getValue()[0]) {
    	case 'L':
    		return Kind.eWChar;
    	case 'u':
    		return  Kind.eChar16;
    	case 'U':
    		return Kind.eChar32;
    	default:
    		return Kind.eChar;
    	}
	}
    
	private IType classifyTypeOfFloatLiteral() {
		final char[] lit= getSuffix();
		final int len= lit.length;
		Kind kind= Kind.eDouble;
		int flags= 0;
		if (len > 0) {
			if (isCompilerSuffix) {
				switch (lit[len - 1]) {
				case 'f': case 'F':
					kind= Kind.eFloat;
					break;
				case 'l': case 'L':
					flags |= IBasicType.IS_LONG;
					break;
				}
			}
			else {
				return getUDLOperatorType();
			}
		}
		return new CPPBasicType(kind, flags, this);
	}

	private IType classifyTypeOfIntLiteral() {
		int makelong= 0;
		boolean unsigned= false;
		final char[] lit= getSuffix();
		int flags= 0;
		
		if (isCompilerSuffix) {
			for (int i= lit.length - 1; i >= 0; i--) {
				final char c= lit[i];
				if (!(c > 'f' && c <= 'z') && !(c > 'F' && c <= 'Z')) {
					break;
				}
				switch (c) {
				case 'u':
				case 'U':
					unsigned = true;
					break;
				case 'l':
				case 'L':
					makelong++;
					break;
				}
			}
	
			if (unsigned) {
				flags |= IBasicType.IS_UNSIGNED;
			}
			
			if (makelong > 1) {
				flags |= IBasicType.IS_LONG_LONG;
			} else if (makelong == 1) {
				flags |= IBasicType.IS_LONG;
			} 
		}
		else if (lit.length > 0) {
			return getUDLOperatorType();
		}
		return new CPPBasicType(Kind.eInt, flags, this);
	}
	
	private int integerLiteral() {
		int i = 0;
		char c = value[i++];
		
		if (c == '0' && i < value.length) {
			// Probably octal/hex/binary
			c = value[i];
			switch ((c | 0x20)) {
			case 'x':
				return probablyHex(i);
			case 'b':
				return probablyBinary(i);
			case '0': case '1': case '2': case '3':
			case '4': case '5': case '6': case '7':
				/* octal-literal:
				*   0
				*   octal-literal octal-digit
				*/
				while (isOctal(c) && i < value.length) {
					c = value[++i];
				}
				break;
			case '.':
				return afterDecimalPoint(i);
			}
			/*
			 * If there is an 8 or 9, then we have a malformed octal
			 */
			if (c == '8' || c == '9') {
				// eat remaining numbers
				c = value[i];
				while (Character.isDigit(c) && i < value.length) {
					c = value[++i];
				}
				return i;
			}
		}
		else if (Character.isDigit(c)) {
			/* decimal-literal :
			*    nonzero-digit         (c has to be this to get into this else)
			*    decimal-literal digit
			*/
			c = value[i];
			while (Character.isDigit(c) && i < value.length) {
				c = value[++i];
			}
			
			if (c == '.') {
				return afterDecimalPoint(i);
			}
			else if ((c | 0x20) == 'e') {
				return exponentPart(i);
			}
		}
		else {
			// Somehow we got called and there wasn't a digit
			// Shouldn't get here
			assert false;
		}
		
		return i;
	}
	
	/*
	 * Called with the expectation that value[i] == '.'
	 */
	private int afterDecimalPoint(int i) {
		char c = value[++i];
		while (Character.isDigit(c) && i < value.length) {
			c = value[++i];
		}
		
		if ((c | 0x20) == 'e') {
			return exponentPart(i);
		}
		
		return i;
	}
	
	/*
	 * Called with the expectation that c == 'e'
	 */
	private int exponentPart(int i) {
		char c = value[++i];
		
		// optional '+' or '-'
		if (c == '+' || c == '-') {
			c = value[++i];
		}
		
		while (Character.isDigit(c) && i < value.length) {
			c = value[++i];
		}
		// If there were no digits following the 'e' then we have
		// D.De or .De which is a UDL on a double
		
		return i--;
	}
	
	// GCC's binary constant notation
	private int probablyBinary(int i) {
		char c = value[++i];
		
		if (c == '1' || c == '0') {
			while (c == '1' || c == '0' && i < value.length) {
				c = value[i++];
			}
			if (Character.isDigit(c)) {
				// UDL can't begin with digit, so this is a malformed binary
				return -1;
			}
			else if (c == '.') {
				// no such thing as binary floating point
				c = value[++i];
				while (Character.isDigit(c) && i < value.length) {
					c = value[i++];
				}
			}
		}
		else {
			// Here we have 0b or 0B
			return i - 1;
		}
		return i;
	}
	
	private int probablyHex(int i) {
		/* hexadecimal-literal
		*   0x hexadecimal-digit
		*   0X hexadecimal-digit
		*   hexadecimal-literal hexadecimal-digit
		*/
		char c = value[++i];
		if (isHexDigit(c)) {
			while (isHexDigit(c) && i < value.length) {
				c = value[++i];
			}
			if (c == '.') {
				// Could be GCC's hex float
				return hexFloatAfterDecimal(i);
			}
			else if ((c | 0x20) == 'p') {
				return hexFloatExponent(i);
			}
		}
		else {
			return i - 1;
		}
		
		return i;
	}
	
	// Assumes value[i] == '.'
	private int hexFloatAfterDecimal(int i) {
		// 0xHHH.
		char c = value[++i];
		if (isHexDigit(c)) {
			while (isHexDigit(c) && i < value.length) {
				c = value[++i];
			}
			
			if ((c | 0x20) == 'p') {
				return hexFloatExponent(i);
			}
			else {
				// The parser is very confused at this point
				// as the expression is 0x1.f
				return -1;
			}
		}
		
		// Probably shouldn't be able to get here
		// we have 0xHHH.
		return -1;
	}
	
	// Assumes image[i] == 'p'
	private int hexFloatExponent(int i) {
		// 0xHH.HH[pP][-+]?DDDD
		char c = value[++i];
		
		if (c == '-' || c == '+') {
			c = value[++i];
		}
		
		if (Character.isDigit(c)) {
			while (Character.isDigit(c) && i < value.length) {
				c = value[++i];
			}
		}
		else {
			return i-1;
		}
		return i;
	}
	
	private boolean isHexDigit(char c) {
		c |= 0x20;
		return ((c <= 'f' && c >= 'a') || (c <= '9' && c >= '0'));
	}
	
	private boolean isOctal(final char c) {
		return c >= '0' && c <= '7';
	}

    /**
     * @deprecated, use {@link #setValue(char[])}, instead.
     */
    @Override
	@Deprecated
	public void setValue(String value) {
        assertNotFrozen();
        this.value = value.toCharArray();
    }

    /**
     * @deprecated use {@link #CPPASTLiteralExpression(int, char[])}, instead.
     */
	@Deprecated
	public CPPASTLiteralExpression(int kind, String value) {
		this(kind, value.toCharArray());
	}
}
