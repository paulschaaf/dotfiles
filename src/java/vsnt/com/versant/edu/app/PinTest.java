package com.versant.edu.app;

import com.versant.edu.persistent.Company;

import com.versant.trans.TransSession;

public class PinTest extends SimpleVersantApp {

	//////////////////////////////////////////////////////////////////////////////
	// CONSTRUCTION & INITIALIZATION

	public static void main(String[] args) {
		new PinTest().mainLoop(args);
	}

	//////////////////////////////////////////////////////////////////////////////
	// ACTIONS

	public void doWorkIn(TransSession session) throws Exception {
		System.out.println("In session on " + this.getDbName() + "...");
		Company company = (Company)session.findRoot("RootCompany");
		System.out.println(company);
		boolean isPinned = session.objectToHandle(company).isPinned();
		System.out.println("\n'isPinned' == " + isPinned);
	}
}
