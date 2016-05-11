package com.versant.edu.persistent;

import com.versant.util.DVector;

public class Company extends BaseModel {
	protected DVector departments = new DVector();

	//////////////////////////////////////////////////////////////////////////////

	// CONSTRUCTION & INITIALIZATION
	public Company(String aName) {
		super(aName);
	}

	//////////////////////////////////////////////////////////////////////////////

	// ACTIONS: DEPARTMENTS
	public void addDepartment(Department dept) {
		departments.addElement(dept);
	}
	public void removeDepartment(Department dept) {
		departments.removeElement(dept);
	}
	public int numberOfDepartments() {
		return departments.size();
	}

	//////////////////////////////////////////////////////////////////////////////

	// PRINTING
	public String toString() {
		return super.toString() + ", Departments: " + this.numberOfDepartments();
	}
}
