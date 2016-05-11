package com.versant.edu.app;

import java.util.Collection;
import java.util.Iterator;

import com.versant.trans.TransSession;

public class Repopulate extends SimpleVersantApp {

	private int repeatCount = 0;

	//////////////////////////////////////////////////////////////////////////////
	// CONSTRUCTION & INITIALIZATION

	public static void main(String[] args) {
		new Repopulate().mainLoop(args);
	}

	public void addArgumentNamesTo(Collection coll) {
		super.addArgumentNamesTo(coll);
		coll.add("repeatCount");
	}

	protected void initializeFrom(Iterator args) {
		super.initializeFrom(args);
		repeatCount = Integer.parseInt((String)args.next());
	}

	//////////////////////////////////////////////////////////////////////////////
	// ACTIONS

	public void doWork() throws Exception {
		TransSession session = new TransSession(dbName);
		CompanySampleMaker sampleMaker = new CompanySampleMaker();
		for(int x = 0;x < repeatCount;x++) {
			session.makePersistent(sampleMaker.newSample());
			session.commit();
		}
		session.endSession();
	}
}
