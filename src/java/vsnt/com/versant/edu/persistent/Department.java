package com.versant.edu.persistent;

import java.util.Enumeration;

import com.versant.util.DVector;
import com.versant.trans.TransSession;

public class Department extends BaseModel {
	protected DVector employees = new DVector();

	//////////////////////////////////////////////////////////////////////////////
	// CONSTRUCTION & INITIALIZATION

	public Department(String aName) {
		super(aName);
	}

	//////////////////////////////////////////////////////////////////////////////
	// ACTIONS: EmployeeS

	public void addEmployee(Employee emp) {
		employees.addElement(emp);
	}
	public void removeEmployee(Employee emp) {
		employees.removeElement(emp);
	}
	public int numberOfEmployees() {
		return employees.size();
	}

	public Enumeration employeesEnum() {
		return employees.elements();
	}

	//////////////////////////////////////////////////////////////////////////////
	// PRINTING

	public String toString() {
		return super.toString() + ", Employees: " + this.numberOfEmployees();
	}
}
