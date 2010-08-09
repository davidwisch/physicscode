! File: src/fortran/putting_interesting.f95

! Sample a function and write out to a file

PROGRAM putting_interesting
	IMPLICIT NONE

	REAL :: min = 1.0, max = 5.0, dx, index
	INTEGER :: points = 250000

	dx = (max - min) / points
	index = min

	OPEN(UNIT=55, FILE="output.txt", STATUS="REPLACE")

	! Sample and write out
	DO WHILE ( index .LE. max)
		WRITE(55, *) index, " ", fun(index)
		index = index + dx
	ENDDO

	CLOSE(UNIT=55)

CONTAINS
	! Define the function
	REAL FUNCTION fun(x)
		REAL :: x, top, bottom

		top = 25.0 * COS(3.0 * x) * SIN(2.0 * x) * LOG(x**2.0)
		bottom = LOG( ABS(x**2.0 - 10.0) )

		fun = top / bottom
		RETURN
	END FUNCTION fun
END PROGRAM putting_interesting
