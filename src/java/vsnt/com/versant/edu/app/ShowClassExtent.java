package com.versant.edu.app;

import java.util.Enumeration;
import java.util.Iterator;
import java.util.Collection;

import com.versant.trans.TransSession;

public class ShowClassExtent extends SimpleVersantApp {

	//////////////////////////////////////////////////////////////////////////////
	// CONSTRUCTION & INITIALIZATION
	Class aClass;

	public static void main(String[] args) {
		new ShowClassExtent().mainLoop(args);
	}
	public void addArgumentNamesTo(Collection coll) {
		super.addArgumentNamesTo(coll);
		coll.add("className");
	}
	protected void initializeFrom(Iterator args) {
		super.initializeFrom(args);
		try {
			aClass = Class.forName((String)args.next());
		}
		catch(ClassNotFoundException ex) {
			aClass = new Object().getClass();
		}
	}

	//////////////////////////////////////////////////////////////////////////////
	// ACTIONS

	public void doWorkIn(TransSession session) {
		Enumeration e = session.select(aClass, null);
		while(e.hasMoreElements()) {
			System.out.println(e.nextElement());
		}
	}
}
