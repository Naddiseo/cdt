/*******************************************************************************
 * Copyright (c) 2013 Ericsson
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     Marc Dumais (Ericsson) - Initial API and implementation (Bug 396268)
 *******************************************************************************/

package org.eclipse.cdt.dsf.gdb.multicorevisualizer.internal.ui.actions;

import org.eclipse.cdt.dsf.gdb.multicorevisualizer.internal.ui.MulticoreVisualizerUIPlugin;
import org.eclipse.cdt.dsf.gdb.multicorevisualizer.internal.ui.view.MulticoreVisualizer;
import org.eclipse.cdt.visualizer.ui.VisualizerAction;

/**
 * @since 1.1
 */
public class EnableLoadMetersAction extends VisualizerAction {

	/** Visualizer instance we're associated with. */
	MulticoreVisualizer m_visualizer = null;
	
	boolean m_enabled = false;

	public EnableLoadMetersAction(boolean enable) {
		m_enabled = enable;
		setText(getTextToDisplay());
		setDescription(MulticoreVisualizerUIPlugin.getString(
				"MulticoreVisualizer.actions.EnableLoadMeter.description")); //$NON-NLS-1$
	}

	/** Dispose method. */
	@Override
	public void dispose()
	{
		m_visualizer = null;
		super.dispose();
	}

	// --- init methods ---

	/** Initializes this action for the specified view. */
	public void init(MulticoreVisualizer visualizer)
	{
		m_visualizer = visualizer;
	}


	// --- methods ---
	
	private String getTextToDisplay() {
		if(m_enabled) {
			return MulticoreVisualizerUIPlugin.getString(
					"MulticoreVisualizer.actions.EnableLoadMeter.Disable.text"); //$NON-NLS-1$
		}
		else {
			return MulticoreVisualizerUIPlugin.getString(
					"MulticoreVisualizer.actions.EnableLoadMeter.Enable.text"); //$NON-NLS-1$
		}
	}

	/** Invoked when action is triggered. */
	@Override
	public void run() {
		if (m_visualizer != null) {
			// toggle enabled state
			m_enabled = !m_enabled;	
			m_visualizer.setLoadMetersEnabled(m_enabled);
			m_visualizer.refresh();
			
			setText(getTextToDisplay());
		}
	}
}
