/*******************************************************************************
 * Copyright (c) 2004, 2012 IBM Corporation and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     IBM - Initial API and implementation
 *     Sergey Prigogin (Google)
 *******************************************************************************/
package org.eclipse.cdt.internal.core.dom.parser.cpp;

import org.eclipse.cdt.core.dom.ast.ASTVisitor;
import org.eclipse.cdt.core.dom.ast.IASTLabelStatement;
import org.eclipse.cdt.core.dom.ast.IASTName;
import org.eclipse.cdt.core.dom.ast.IASTNode;
import org.eclipse.cdt.core.dom.ast.IASTStatement;
import org.eclipse.cdt.internal.core.dom.parser.ASTAttributeOwner;
import org.eclipse.cdt.internal.core.dom.parser.IASTAmbiguityParent;

/**
 * @author jcamelon
 */
public class CPPASTLabelStatement extends ASTAttributeOwner
		implements IASTLabelStatement, IASTAmbiguityParent {
    private IASTName name;
    private IASTStatement nestedStatement;
    
    public CPPASTLabelStatement() {
	}

	public CPPASTLabelStatement(IASTName name, IASTStatement nestedStatement) {
		setName(name);
		setNestedStatement(nestedStatement);
	}
	
	@Override
	public CPPASTLabelStatement copy() {
		return copy(CopyStyle.withoutLocations);
	}

	@Override
	public CPPASTLabelStatement copy(CopyStyle style) {
		CPPASTLabelStatement copy = new CPPASTLabelStatement();
		copy.setName(name == null ? null : name.copy(style));
		copy.setNestedStatement(nestedStatement == null ? null : nestedStatement.copy(style));
		return copy(copy, style);
	}

	@Override
	public IASTName getName() {
        return name;
    }

    @Override
	public void setName(IASTName name) {
        assertNotFrozen();
        this.name = name;
        if (name != null) {
			name.setParent(this);
			name.setPropertyInParent(NAME);
		}
    }

    @Override
	public boolean accept(ASTVisitor action) {
        if (action.shouldVisitStatements) {
		    switch (action.visit(this)) {
	            case ASTVisitor.PROCESS_ABORT: return false;
	            case ASTVisitor.PROCESS_SKIP: return true;
	            default: break;
	        }
		}

        if (!acceptByAttributes(action)) return false;
        if (name != null && !name.accept(action)) return false;
        if (nestedStatement != null && !nestedStatement.accept(action)) return false;
        
        if (action.shouldVisitStatements) {
		    switch (action.leave(this)) {
	            case ASTVisitor.PROCESS_ABORT: return false;
	            case ASTVisitor.PROCESS_SKIP: return true;
	            default: break;
	        }
		}
        return true;
    }

	@Override
	public int getRoleForName(IASTName n) {
		if (n == name) return r_declaration;
		return r_unclear;
	}

    @Override
	public IASTStatement getNestedStatement() {
        return nestedStatement;
    }

    @Override
	public void setNestedStatement(IASTStatement s) {
        assertNotFrozen();
        nestedStatement = s;
        if (s != null) {
			s.setParent(this);
			s.setPropertyInParent(NESTED_STATEMENT);
		}
    }

    @Override
	public void replace(IASTNode child, IASTNode other) {
        if (child == nestedStatement) {
            other.setParent(this);
            other.setPropertyInParent(child.getPropertyInParent());
            setNestedStatement((IASTStatement) other);
        }
    }
}
