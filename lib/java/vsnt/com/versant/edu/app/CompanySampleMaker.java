package com.versant.edu.app;

import com.versant.edu.persistent.*;

public class CompanySampleMaker {

	//////////////////////////////////////////////////////////////////////////////
	// CONSTRUCTION & INITIALIZATION

	public CompanySampleMaker() {
		super();
	}

	//////////////////////////////////////////////////////////////////////////////
	// ACTIONS

	public Company newSample() {
		Company company = new Company("ACME");
		this.addDepartmentsTo(company, 10);
		return company;
	}

	public void addDepartmentsTo(Company company, int count) {
		Department dept;
		for(int i=0; i<count; i++) {
			dept = new Department("Dept-" + i);
			company.addDepartment(dept);
			this.addEmployeesTo(dept, 10);
		}
	}

	public void addEmployeesTo(Department dept, int count) {
		Employee emp;
		for(int i=0; i<count; i++) {
			emp = new Employee("Emp-" + i);
			dept.addEmployee(emp);
		}
	}
}
