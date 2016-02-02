package com.versant.edu.app;

import java.util.Collection;
import java.util.Iterator;

import com.versant.trans.TransSession;
import com.versant.edu.lib.Closure;

abstract public class SimpleVersantApp extends SimpleApp {

	private TransSession session;
	private String dbName;

	//////////////////////////////////////////////////////////////////////////////
	// CONSTRUCTION & INITIALIZATION

	protected void addArgumentNamesTo(Collection coll) {
		coll.add("dbName");
	}

	protected void initializeFrom(Iterator args) {
		dbName = (String)args.next();
	}

	//////////////////////////////////////////////////////////////////////////////
	// SESSION

	public void inSessionDo(Closure closure) {
		try {
			session = new TransSession(dbName);
			closure.exec(session, this);
		}
		catch(Exception ex) {
			System.out.println("Exception caught!");
			ex.printStackTrace();
			session.rollback();
		}
		finally {
			session.endSession();
			session = null;
		}
	}

	//////////////////////////////////////////////////////////////////////////////
	// ACTIONS

	public void doWork() {
		this.inSessionDo(new Closure()  {
			public void exec(Object session, Object app) throws Exception {
				((SimpleVersantApp)app).doWorkIn((TransSession)session);
			}
		});
	}

	abstract public void doWorkIn(TransSession session) throws Exception;

	//////////////////////////////////////////////////////////////////////////////
	// ACCESSORS

	public TransSession getSession() {
		return session;
	}

	public String getDbName() {
		return dbName;
	}
}
