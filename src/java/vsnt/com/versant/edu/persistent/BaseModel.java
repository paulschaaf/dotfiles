package com.versant.edu.persistent;

public abstract class BaseModel {
	protected String name = new String();
	
	//////////////////////////////////////////////////////////////////////////////
	
	// CONSTRUCTION & INITIALIZATION
	public BaseModel(String aName) {
		name = aName;
	}
	
	//////////////////////////////////////////////////////////////////////////////
	
	// PERSISTENT STATE
	public String toString() {
		return name;
	}
	public void setName(String aName) {
		name = aName;
	}
}
