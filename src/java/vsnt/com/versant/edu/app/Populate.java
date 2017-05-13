package com.versant.edu.app;

import com.versant.trans.TransSession;

public class Populate extends SimpleVersantApp {

	//////////////////////////////////////////////////////////////////////////////
	// CONSTRUCTION & INITIALIZATION

	public static void main(String[] args) {
		new Populate().mainLoop(args);
	}

	//////////////////////////////////////////////////////////////////////////////
	// ACTIONS

	public void doWorkIn(TransSession session) throws Exception {
		CompanySampleMaker sampleMaker = new CompanySampleMaker();
		session.makeRoot("RootCompany", sampleMaker.newSample());
	}
}
