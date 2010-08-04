! File: src/fortran/basic_control_andor.f95

! Demonstrates the use of .AND. and .OR. statements.

PROGRAM basic_control_andor
	IMPLICIT NONE

	CHARACTER(25) :: user_name

	WRITE(*, '(A)', ADVANCE="NO") "What is your name? "
	READ *, user_name

	IF ( TRIM(user_name) .EQ. "John" .OR. TRIM(user_name) .EQ. "Steve" ) THEN
		PRINT *, "Your name is John or Steve"
	ELSE IF ( TRIM(user_name) .NE. "Jenny" .AND. LEN_TRIM(user_name) .LE. 6 ) THEN
		PRINT *, "Your name is 6 or letters and is not Jenny"
	ELSE
		PRINT *, "Your name doesn't fit our random conditions"
	END IF
END PROGRAM basic_control_andor
