/*******************************************************************************
 * Copyright (c) 2012 Sage Electronic Engineering, LLC. and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *    Jason Litton (Sage Electronic Engineering, LLC) - initial API and implementation
 *******************************************************************************/

package org.eclipse.cdt.dsf.gdb.internal;

import java.util.Hashtable;

import org.eclipse.osgi.service.debug.DebugOptions;
import org.eclipse.osgi.service.debug.DebugOptionsListener;
import org.eclipse.osgi.service.debug.DebugTrace;
import org.osgi.framework.BundleContext;

/**
 *
 * @since 4.1
 *
 */
public class GdbDebugOptions implements DebugOptionsListener {

	public static final String DEBUG_FLAG = "org.eclipse.cdt.dsf.gdb/debug"; //$NON-NLS-1$
	public static final String DEBUG_TIMEOUTS_FLAG = "org.eclipse.cdt.dsf.gdb/debug/timeouts"; //$NON-NLS-1$

	public static boolean DEBUG = false;
	public static boolean DEBUG_TIMEOUTS = false;

	/**
	 * The {@link DebugTrace} object to print to OSGi tracing
	 */
	private static DebugTrace fgDebugTrace;

	/**
	 * Constructor
	 */
	public GdbDebugOptions(BundleContext context) {
		Hashtable<String, String> props = new Hashtable<String, String>(2);
		props.put(org.eclipse.osgi.service.debug.DebugOptions.LISTENER_SYMBOLICNAME, GdbPlugin.getUniqueIdentifier());
		context.registerService(DebugOptionsListener.class.getName(), this, props);
	}


	@Override
	public void optionsChanged(DebugOptions options) {
		fgDebugTrace = options.newDebugTrace(GdbPlugin.getUniqueIdentifier());
		DEBUG = options.getBooleanOption(DEBUG_FLAG, false);
		DEBUG_TIMEOUTS = options.getBooleanOption(DEBUG_TIMEOUTS_FLAG, false);
	}

	/**
	 * Prints the given message to System.out and to the OSGi tracing (if started)
	 * @param option the option or <code>null</code>
	 * @param message the message to print or <code>null</code>
	 * @param throwable the {@link Throwable} or <code>null</code>
	 */
	public static void trace(String option, String message, Throwable throwable) {
		//copy the message to a new string, then print the new string to the
		//console in blocks of 100 characters.
		String systemPrintableMessage = message; 
		while (systemPrintableMessage.length() > 100) {
			String partial = systemPrintableMessage.substring(0, 100); 
			systemPrintableMessage = systemPrintableMessage.substring(100);
			System.out.println(partial + "\\"); //$NON-NLS-1$
		}
		//then pass the original message to be traced into a file
		if(fgDebugTrace != null) {
			fgDebugTrace.trace(option, message, throwable);
		}
	}

	/**
	 * Prints the given message to System.out and to the OSGi tracing (if enabled)
	 *
	 * @param message the message or <code>null</code>
	 */
	public static void trace(String message) {
		trace(null, message, null);
	}

}
