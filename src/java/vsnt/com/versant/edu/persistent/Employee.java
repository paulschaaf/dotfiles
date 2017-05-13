package com.versant.edu.persistent;

public class Employee extends BaseModel {

	protected String phone = new String();

	//////////////////////////////////////////////////////////////////////////////

	// CONSTRUCTION & INITIALIZATION
	public Employee(String aName) {
		super(aName);
	}
	public Employee(String aName, String aPhone) {
		super(aName);
		phone = aPhone;
	}

	//////////////////////////////////////////////////////////////////////////////

	// ATTRIBUTES
	public void setPhone(String aPhone) {
		phone = aPhone;
	}
	public String getPhone() {
		return phone;
	}

	//////////////////////////////////////////////////////////////////////////////

	// PRINTING
	public String toString() {
		return super.toString() + ", Phone: " + this.getPhone();
	}
}
