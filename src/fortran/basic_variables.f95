! File: src/python/basic_variables.f95

! Program demonstrates declaration and use of basic variables

PROGRAM basic_variables
	IMPLICIT NONE ! require explicit variable declaration

	CHARACTER(LEN=20) :: firstname="John", lastname="Doe"
	INTEGER :: age=45
	REAL :: height=1.8 ! meters
	
	! We can change the value of a variable simply with
	age = 50 ! John's age is now 50
	age = 45 ! John's age is back to 45

	! NOTE ON 'TRIM':
	!
	! Because our CHARACTERS were declared with a length of 20
	! but the text in them is less than 20, empty space is left
	! at the end of the string; TRIM removes it.

	PRINT *, TRIM(firstname), " ", TRIM(lastname), " is", age, "years old and is", height, "m tall."

	! Example without using TRIM
	PRINT *, firstname, " ", lastname, " is", age, "years old and is", height, "m tall."
END PROGRAM basic_variables
