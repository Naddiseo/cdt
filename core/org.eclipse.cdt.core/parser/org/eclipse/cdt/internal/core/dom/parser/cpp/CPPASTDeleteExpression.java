/*******************************************************************************
 * Copyright (c) 2004, 2009 IBM Corporation and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     John Camelon (IBM) - Initial API and implementation
 *******************************************************************************/
package org.eclipse.cdt.internal.core.dom.parser.cpp;

import java.util.ArrayList;
import java.util.List;

import org.eclipse.cdt.core.dom.ast.ASTVisitor;
import org.eclipse.cdt.core.dom.ast.DOMException;
import org.eclipse.cdt.core.dom.ast.IASTExpression;
import org.eclipse.cdt.core.dom.ast.IASTImplicitName;
import org.eclipse.cdt.core.dom.ast.IPointerType;
import org.eclipse.cdt.core.dom.ast.IType;
import org.eclipse.cdt.core.dom.ast.cpp.ICPPASTDeleteExpression;
import org.eclipse.cdt.core.dom.ast.cpp.ICPPClassType;
import org.eclipse.cdt.core.dom.ast.cpp.ICPPFunction;
import org.eclipse.cdt.internal.core.dom.parser.ASTNode;
import org.eclipse.cdt.internal.core.dom.parser.cpp.semantics.CPPSemantics;
import org.eclipse.cdt.internal.core.dom.parser.cpp.semantics.SemanticUtil;


public class CPPASTDeleteExpression extends ASTNode implements ICPPASTDeleteExpression {

    private IASTExpression operand;
    private boolean isGlobal;
    private boolean isVectored;

    private IASTImplicitName[] implicitNames = null;
    
    
    public CPPASTDeleteExpression() {
	}

	public CPPASTDeleteExpression(IASTExpression operand) {
		setOperand(operand);
	}
	
	public CPPASTDeleteExpression(CPPASTDeleteExpression from) {
		setOperand(from.operand);
	}
	
	public CPPASTDeleteExpression copy() {
		CPPASTDeleteExpression copy = new CPPASTDeleteExpression(operand == null ? null : operand.copy());
		copy.isGlobal = isGlobal;
		copy.isVectored = isVectored;
		copy.setOffsetAndLength(this);
		return copy;
	}
	
	public IASTExpression getOperand() {
        return operand;
    }

    public void setOperand(IASTExpression expression) {
        assertNotFrozen();
        operand = expression;
        if (expression != null) {
			expression.setParent(this);
			expression.setPropertyInParent(OPERAND);
		}
    }

    public void setIsGlobal(boolean global) {
        assertNotFrozen();
        isGlobal = global;
    }

    public boolean isGlobal() {
        return isGlobal;
    }

    public void setIsVectored(boolean vectored) {
        assertNotFrozen();
        isVectored = vectored;
    }

    public boolean isVectored() {
        return isVectored;
    }
    
    /**
     * Try to resolve both the destructor and the operator delete.
     */
    public IASTImplicitName[] getImplicitNames() {
    	if(implicitNames == null) {
    		ICPPClassType nestedType = getNestedClassType();
	    	if(nestedType == null)
	    		return implicitNames = IASTImplicitName.EMPTY_NAME_ARRAY;
	    	
	    	List<IASTImplicitName> names = new ArrayList<IASTImplicitName>();
	    	
	    	if(!isVectored) {
		    	ICPPFunction destructor = CPPSemantics.findDestructor(this, nestedType);
		    	if(destructor != null) {
		    		CPPASTImplicitName destructorName = new CPPASTImplicitName(destructor.getNameCharArray(), this);
		    		destructorName.setBinding(destructor);
		    		destructorName.computeOperatorOffsets(operand, false);
		    		names.add(destructorName);
		    	}
	    	}
	    	
	    	if(!isGlobal) {
		    	ICPPFunction deleteOperator = findOperatorFunction(nestedType);
		    	if(deleteOperator != null) {
		    		CPPASTImplicitName deleteName = new CPPASTImplicitName(deleteOperator.getNameCharArray(), this);
		    		deleteName.setOperator(true);
		    		deleteName.setBinding(deleteOperator);
		    		deleteName.computeOperatorOffsets(operand, false);
		    		names.add(deleteName);
		    	}
	    	}
	    	
	    	if(names.isEmpty())
	    		implicitNames = IASTImplicitName.EMPTY_NAME_ARRAY;
	    	else
	    		implicitNames = names.toArray(new IASTImplicitName[names.size()]);
    	}
    	
    	return implicitNames;    	
	}

    
    private ICPPClassType getNestedClassType() {
    	IType type1 = operand.getExpressionType();
    	IType ultimateType1 = SemanticUtil.getUltimateTypeUptoPointers(type1);
    	if(ultimateType1 instanceof IPointerType) {
    		try {
				IType classType = ((IPointerType)ultimateType1).getType();
				if(classType instanceof ICPPClassType)
					return (ICPPClassType) classType;
			} catch (DOMException e) {
				return null;
			}
    	}
		return null;
    }
    
    // TODO this code is repeated in too many places
    private ICPPFunction findOperatorFunction(IType type) {
    	if(type instanceof ICPPClassType) {
			ICPPFunction operator = CPPSemantics.findOperator(this, (ICPPClassType) type);
			if(operator != null)
				return operator;
			return CPPSemantics.findOverloadedOperator(this); 
		}
    	
    	return null;
    }
    
    @Override
	public boolean accept( ASTVisitor action ){
        if( action.shouldVisitExpressions ){
		    switch( action.visit( this ) ){
	            case ASTVisitor.PROCESS_ABORT : return false;
	            case ASTVisitor.PROCESS_SKIP  : return true;
	            default : break;
	        }
		}
        
        if(action.shouldVisitImplicitNames) { 
        	for(IASTImplicitName name : getImplicitNames()) {
        		if(!name.accept(action)) return false;
        	}
        }
        
        if( operand != null ) if( !operand.accept( action ) ) return false;
        
        if( action.shouldVisitExpressions ){
		    switch( action.leave( this ) ){
	            case ASTVisitor.PROCESS_ABORT : return false;
	            case ASTVisitor.PROCESS_SKIP  : return true;
	            default : break;
	        }
		}
        return true;
    }
    
    public IType getExpressionType() {
    	return CPPSemantics.VOID_TYPE;
    }
}
