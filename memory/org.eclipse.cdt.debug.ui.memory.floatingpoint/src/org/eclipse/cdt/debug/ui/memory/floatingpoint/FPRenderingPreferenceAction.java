/*******************************************************************************
 * Copyright (c) 2006, 2010, 2012 Wind River Systems, Inc. and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     Ted R Williams (Wind River Systems, Inc.) - initial implementation
 *     Randy Rohrbach (Wind River Systems, Inc.) - Copied and modified to create the floating point plugin
 *******************************************************************************/

package org.eclipse.cdt.debug.ui.memory.floatingpoint;

import org.eclipse.jface.action.IAction;
import org.eclipse.jface.preference.IPreferenceNode;
import org.eclipse.jface.preference.IPreferencePage;
import org.eclipse.jface.preference.PreferenceDialog;
import org.eclipse.jface.preference.PreferenceManager;
import org.eclipse.jface.preference.PreferenceNode;
import org.eclipse.jface.window.Window;
import org.eclipse.swt.custom.BusyIndicator;
import org.eclipse.ui.IViewActionDelegate;
import org.eclipse.ui.IViewPart;
import org.eclipse.ui.actions.ActionDelegate;

public class FPRenderingPreferenceAction extends ActionDelegate implements IViewActionDelegate
{

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.ui.IActionDelegate#run(org.eclipse.jface.action.IAction)
     */
    @Override
    public void run(IAction action)
    {
        IPreferencePage page = new FPRenderingPreferencePage();
        showPreferencePage("org.eclipse.cdt.debug.ui.memory.floatingpoint.FPRenderingPreferencePage", page); //$NON-NLS-1$
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.ui.IViewActionDelegate#init(org.eclipse.ui.IViewPart)
     */
    @Override
    public void init(IViewPart view)
    {
    }

    protected void showPreferencePage(String id, IPreferencePage page)
    {
        final IPreferenceNode targetNode = new PreferenceNode(id, page);

        PreferenceManager manager = new PreferenceManager();
        manager.addToRoot(targetNode);
        final PreferenceDialog dialog = new PreferenceDialog(FPRenderingPlugin.getShell(), manager);
        final boolean[] result = new boolean[] { false };
        BusyIndicator.showWhile(FPRenderingPlugin.getStandardDisplay(), new Runnable()
        {
            @Override
            public void run()
            {
                dialog.create();
                dialog.setMessage(targetNode.getLabelText());
                result[0] = (dialog.open() == Window.OK);
            }
        });
    }
}
