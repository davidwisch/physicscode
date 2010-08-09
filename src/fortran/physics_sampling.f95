! File: src/fortran/physics_sampling.f95

! Samples cos(x^2) 1000 times storing the data in an array then printing it out

PROGRAM physics_sampling
	IMPLICIT NONE

	REAL, DIMENSION(1000, 2) :: values
	REAL :: limit, dx, val
	INTEGER :: samples = 1000, i

	! Init some variables
	limit = 4.0 * ATAN(1.0) ! Pi
	dx = limit / samples

	! Sample the function
	DO i = 0, samples
		val = cosXSquared(i * dx)
		values(i, 1) = i * dx
		values(i, 2) = val
	END DO

	! Print the results
	DO i = 0, samples
		PRINT *, "Value at: ", values(i, 1), " is: ", values(i, 2)
	END DO


CONTAINS
	! Define the function
	REAL FUNCTION cosXSquared(x)
		REAL :: x

		cosXSquared = COS(x**2)
		RETURN
	END FUNCTION cosXSquared
END PROGRAM physics_sampling
