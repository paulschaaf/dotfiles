package com.versant.edu.app;
import com.versant.trans.TransSession;

public class DbFree extends SimpleVersantApp {

	//////////////////////////////////////////////////////////////////////////////
	// CONSTRUCTION & INITIALIZATION

	public static void main(String[] args) {
		new DbFree().mainLoop(args);
	}

	//////////////////////////////////////////////////////////////////////////////
	// ACTIONS

	public void doWorkIn(TransSession session) throws Exception {
		System.out.println("free kb: " + session.diskFreeKb());
		System.out.println("free Mb: " + (session.diskFreeKb() / 1024));
		System.out.println("free  %: " + session.diskFreePercent());
	}

}
