package com.versant.edu.app;

import java.util.Arrays;
import java.util.Collection;
import java.util.Iterator;
import java.util.Vector;

abstract public class SimpleApp {

	//////////////////////////////////////////////////////////////////////////////
	// CONSTRUCTION & INITIALIZATION

	protected void mainLoop(String[] args) {
		this.initializeFrom(args);
		this.doWork();
	}

	protected Collection argumentNames() {
		Vector vec = new Vector(1);
		this.addArgumentNamesTo(vec);
		return vec;
	}

	protected void addArgumentNamesTo(Collection coll) {
	}

	public String usageString() {
		StringBuffer aBuffer = new StringBuffer();
		aBuffer.append("Usage: java ").append(this.getClass().getName());
		Iterator iter = this.argumentNames().iterator();
		while(iter.hasNext()) {
			aBuffer.append(" <").append((String)iter.next()).append(">");
		}
		return aBuffer.toString();
	}

	protected void initializeFrom(String[] args) {
		if(args.length < this.argumentNames().size()) {
			System.err.println(this.usageString());
			System.exit(1);
		}
		Iterator iter = Arrays.asList(args).iterator();
		this.initializeFrom(iter);
	}

	abstract protected void initializeFrom(Iterator args);

	//////////////////////////////////////////////////////////////////////////////
	// ACTIONS

	abstract public void doWork();
}
