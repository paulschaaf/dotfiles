package com.versant.edu.app;

import java.io.BufferedReader;
import java.io.InputStreamReader;

import com.versant.trans.TransSession;

public class HoldSession extends SimpleVersantApp {
	protected BufferedReader buffer;

	//////////////////////////////////////////////////////////////////////////////
	// CONSTRUCTION & INITIALIZATION

	public static void main(String[] args) {
		new HoldSession().mainLoop(args);
	}

	//////////////////////////////////////////////////////////////////////////////
	// ACTIONS

	public void doWorkIn(TransSession session) throws Exception {
		System.out.println("In session on " + this.getDbName() + "...");
		Object company = session.findRoot("RootCompany");
		System.out.println("\nPress <enter> to end session.");
		buffer = new BufferedReader(new InputStreamReader(System.in));
		buffer.readLine();
	}
}
