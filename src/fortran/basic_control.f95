! File: src/fortran/basic_control.f95

! Uses control statments to determine is a number is positive, negative, or zero

PROGRAM basic_control
	IMPLICIT NONE

	INTEGER :: num

	WRITE(*, '(A)', ADVANCE="NO") "Input a Number: "
	READ *, num
	
	IF ( num .GT. 0 ) THEN
		PRINT *, num, " is positive"
	ELSE IF (num .LT. 0 ) THEN
		PRINT *, num, " is negative"
	ELSE
		PRINT *, num, " is zero"
	END IF
END PROGRAM basic_control
