! File: src/fortran/basic_loops_while.f95

! Demonstrate the use of a basic while loop

PROGRAM basic_loops_while
	IMPLICIT NONE

	INTEGER :: index = 0

	DO WHILE ( index .LT. 25 )
		PRINT *, "Index is: ", index
		index = index + 1
	ENDDO
END PROGRAM basic_loops_while
