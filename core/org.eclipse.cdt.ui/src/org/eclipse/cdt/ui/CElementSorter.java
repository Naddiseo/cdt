package org.eclipse.cdt.ui;

/*
 * (c) Copyright IBM Corp. 2000, 2001.
 * All Rights Reserved.
 */
 
import org.eclipse.cdt.core.model.CModelException;
import org.eclipse.cdt.core.model.CoreModel;
import org.eclipse.cdt.core.model.IArchive;
import org.eclipse.cdt.core.model.IArchiveContainer;
import org.eclipse.cdt.core.model.IBinary;
import org.eclipse.cdt.core.model.IBinaryContainer;
import org.eclipse.cdt.core.model.ICContainer;
import org.eclipse.cdt.core.model.ICElement;
import org.eclipse.cdt.core.model.ICModel;
import org.eclipse.cdt.core.model.ICProject;
import org.eclipse.cdt.core.model.IFunction;
import org.eclipse.cdt.core.model.IFunctionDeclaration;
import org.eclipse.cdt.core.model.IInclude;
import org.eclipse.cdt.core.model.ILibraryReference;
import org.eclipse.cdt.core.model.IMacro;
import org.eclipse.cdt.core.model.IMethod;
import org.eclipse.cdt.core.model.IMethodDeclaration;
import org.eclipse.cdt.core.model.INamespace;
import org.eclipse.cdt.core.model.ISourceRoot;
import org.eclipse.cdt.core.model.ITranslationUnit;
import org.eclipse.cdt.core.model.IUsing;
import org.eclipse.cdt.core.model.IVariable;
import org.eclipse.cdt.core.model.IVariableDeclaration;
import org.eclipse.core.resources.IContainer;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.IStorage;
import org.eclipse.core.runtime.IAdaptable;
import org.eclipse.core.runtime.IPath;
import org.eclipse.jface.viewers.ContentViewer;
import org.eclipse.jface.viewers.IBaseLabelProvider;
import org.eclipse.jface.viewers.ILabelProvider;
import org.eclipse.jface.viewers.Viewer;
import org.eclipse.jface.viewers.ViewerSorter;
import org.eclipse.ui.model.IWorkbenchAdapter;

/**
 *	A sorter to sort the file and the folders in the C viewer in the following order:
 * 	1st Project
 * 	2nd BinaryContainer
 *  3nd ArchiveContainer
 *  4  Folder
 *  5  C File
 *  6 the reset
 */
public class CElementSorter extends ViewerSorter { 

	private static final int CMODEL = 0;
	private static final int PROJECTS = 10;
	private static final int BINARYCONTAINER = 11;
	private static final int ARCHIVECONTAINER = 12;
	private static final int SOURCEROOTS = 13;
	private static final int CCONTAINERS = 14;
	private static final int TRANSLATIONUNIT_HEADERS = 15;
	private static final int TRANSLATIONUNIT_SOURCE = 16;
	private static final int TRANSLATIONUNITS = 17;
	private static final int BINARIES = 18;
	private static final int ARCHIVES = 19;
	private static final int LIBRARYREFERENCES = 20;
	
	private static final int INCLUDES = 30;
	private static final int MACROS = 31;
	private static final int NAMESPACES = 32;
	private static final int USINGS = 33;
	private static final int VARIABLEDECLARATIONS = 34;
	private static final int FUNCTIONDECLARATIONS = 35;
	private static final int VARIABLES = 36;
	private static final int VARIABLES_RESERVED = 37;
	private static final int VARIABLES_SYSTEM = 38;
	private static final int FUNCTIONS = 39;
	private static final int FUNCTIONS_RESERVED = 40;
	private static final int FUNCTIONS_SYSTEM = 41;
	private static final int METHODDECLARATIONS = 42;

	private static final int CELEMENTS = 100;
	private static final int CELEMENTS_RESERVED = 101;
	private static final int CELEMENTS_SYSTEM = 102;

	private static final int RESOURCEFOLDERS= 200;
	private static final int RESOURCES= 201;
	private static final int STORAGE= 202;
	private static final int OTHERS= 500;

	public int category (Object element) {
		if (element instanceof ICModel) {
			return CMODEL;
		} else if (element instanceof ICProject) {
			return PROJECTS;
		} else if (element instanceof ISourceRoot) {
			return SOURCEROOTS;
		} else if (element instanceof IBinaryContainer) {
			return BINARYCONTAINER;
		} else if (element instanceof IArchiveContainer) {
			return ARCHIVECONTAINER;
		} else if (element instanceof ICContainer) {
			return CCONTAINERS;
		} else if (element instanceof ITranslationUnit) {
			IResource res = null;
			res = ((ITranslationUnit)element).getUnderlyingResource();
			if (res != null) {
				String ext = res.getFileExtension();
				if (ext != null) {
					String[] headers = CoreModel.getDefault().getHeaderExtensions();
					for (int i = 0; i < headers.length; i++) {
						if (ext.equals(headers[i])) {
							return TRANSLATIONUNIT_HEADERS;
						}
					}
					String[] sources = CoreModel.getDefault().getSourceExtensions();
					for (int i = 0; i < sources.length; i++) {
						if (ext.equals(sources[i])) {
							return TRANSLATIONUNIT_SOURCE;
						}
					}					
				}
			}
			return TRANSLATIONUNITS;
		} else if (element instanceof IInclude) {
			return INCLUDES;
		} else if (element instanceof IMacro) {
			return MACROS;
		} else if (element instanceof INamespace) {
			return NAMESPACES;
		} else if (element instanceof IUsing) {
			return USINGS;
		} else if (element instanceof IFunctionDeclaration && ! (element instanceof IFunction)) {
			return FUNCTIONDECLARATIONS;
		} else if (element instanceof IMethodDeclaration && !(element instanceof IMethod)) {
			return METHODDECLARATIONS;
		} else if (element instanceof IVariableDeclaration) {
			return VARIABLEDECLARATIONS;
		} else if (element instanceof IVariable) {
			String name = ((ICElement)element).getElementName();
			if (name.startsWith("__")) { //$NON-NLS-1$
				return VARIABLES_SYSTEM;
			}
			if (name.charAt(0) == '_') {
				return VARIABLES_RESERVED;
			}
			return VARIABLES;
		} else if (element instanceof IFunction) {
			String name = ((ICElement)element).getElementName();
			if (name.startsWith("__")) { //$NON-NLS-1$
				return FUNCTIONS_SYSTEM;
			}
			if (name.charAt(0) == '_') {
				return FUNCTIONS_RESERVED;
			}
			return FUNCTIONS;
		} else if (element instanceof IArchive) {
			return ARCHIVES;
		} else if (element instanceof IBinary) {
			return BINARIES;
		} else if (element instanceof ILibraryReference) {
			return LIBRARYREFERENCES;
		} else if (element instanceof ICElement) {
			String name = ((ICElement)element).getElementName();
			if (name.startsWith("__")) { //$NON-NLS-1$
				return CELEMENTS_SYSTEM;
			}
			if (name.charAt(0) == '_') {
				return CELEMENTS_RESERVED;
			}
			return CELEMENTS;
		} else if (element instanceof IFile) {
			return RESOURCES;
		} else if (element instanceof IProject) {
			return PROJECTS;
		} else if (element instanceof IContainer) {
			return RESOURCEFOLDERS;
		} else if (element instanceof IStorage) {
			return STORAGE;
		}
		return OTHERS;
	}
	/* (non-Javadoc)
	 * @see org.eclipse.jface.viewers.ViewerSorter#compare(org.eclipse.jface.viewers.Viewer, java.lang.Object, java.lang.Object)
	 */
	public int compare(Viewer viewer, Object e1, Object e2) {
		int cat1 = category(e1);
		int cat2 = category(e2);

		if (cat1 != cat2)
			return cat1 - cat2;

		// cat1 == cat2

		if (cat1 == PROJECTS) {
			IWorkbenchAdapter a1= (IWorkbenchAdapter)((IAdaptable)e1).getAdapter(IWorkbenchAdapter.class);
			IWorkbenchAdapter a2= (IWorkbenchAdapter)((IAdaptable)e2).getAdapter(IWorkbenchAdapter.class);
			return getCollator().compare(a1.getLabel(e1), a2.getLabel(e2));
		}

		if (cat1 == SOURCEROOTS) {
			ISourceRoot root1= getSourceRoot(e1);
			ISourceRoot root2= getSourceRoot(e2);
			if (root1 == null) {
				if (root2 == null) {
					return 0;
				} else {
					return 1;
				}
			} else if (root2 == null) {
				return -1;
			}			
			if (!root1.getPath().equals(root2.getPath())) {
				int p1= getPathEntryIndex(root1);
				int p2= getPathEntryIndex(root2);
				if (p1 != p2) {
					return p1 - p2;
				}
			}
		}

		// non - c resources are sorted using the label from the viewers label provider
		if (cat1 == RESOURCES || cat1 == RESOURCEFOLDERS || cat1 == STORAGE || cat1 == OTHERS) {
			return compareWithLabelProvider(viewer, e1, e2);
		}
		
		String name1;
		String name2;

		if (e1 instanceof ICElement) {
			name1 = ((ICElement)e1).getElementName();
		} else {
			name1 = e1.toString();
		}
		if (e2 instanceof ICElement) {
			name2 = ((ICElement)e2).getElementName();
		} else {
			name2 = e2.toString();
		}
		return getCollator().compare(name1, name2);		
	}

	private ISourceRoot getSourceRoot(Object element) {
		ICElement celement = (ICElement)element;
		while (! (celement instanceof ISourceRoot) && celement != null) {
			celement = celement.getParent();
		}
		return (ISourceRoot)celement;
	}

	private int compareWithLabelProvider(Viewer viewer, Object e1, Object e2) {
		if (viewer == null || !(viewer instanceof ContentViewer)) {
			IBaseLabelProvider prov = ((ContentViewer) viewer).getLabelProvider();
			if (prov instanceof ILabelProvider) {
				ILabelProvider lprov= (ILabelProvider) prov;
				String name1 = lprov.getText(e1);
				String name2 = lprov.getText(e2);
				if (name1 != null && name2 != null) {
					return getCollator().compare(name1, name2);
				}
			}
		}
		return 0; // can't compare
	}

	private int getPathEntryIndex(ISourceRoot root) {
		try {
			IPath rootPath= root.getPath();
			ISourceRoot[] roots= root.getCProject().getSourceRoots();
			for (int i= 0; i < roots.length; i++) {
				if (roots[i].getPath().equals(rootPath)) {
					return i;
				}
			}
		} catch (CModelException e) {
		}

		return Integer.MAX_VALUE;
	}

}
