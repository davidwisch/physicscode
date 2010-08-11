! File src/fortran/physics_numerical_euler.f95

! Simulates an object being launched at a particular angle given an initial velocity using the forward Euler method.

PROGRAM physics_numerical_euler
	IMPLICIT NONE

	REAL :: ANGLE, V0, DT, G, H_DAMP ! to be constants
	REAL :: t, x, y, vx, vy, PI
	PARAMETER (ANGLE = 30.0, V0 = 112.0, DT = 0.01, G = -9.8, H_DAMP = 2.0)

	PI = 4.0 * ATAN(1.0)

	t = 0.0 ! time (seconds)
	x = 0.0 ! x-position (m)
	y = 0.0 ! y-position (m)

	vx = V0 * SIN((PI / 180.0) * ANGLE)
	vy = V0 * SIN((PI / 180.0) * ANGLE)

	OPEN(UNIT=55, FILE="euler.txt", STATUS="REPLACE")
	DO
		vx = vel(vx, H_DAMP, DT)
		vy = vel(vy, G, DT)

		x = pos(x, vx, DT)
		y = pos(y, vy, DT)

		t = t + DT

		IF ( y .LE. 0 ) THEN
			EXIT
		ENDIF

		WRITE(55, *) t, " ", x, " ", y, " ", vx, " ", vy
	ENDDO
	CLOSE(UNIT=55)

CONTAINS
	! Calculates velocity
	REAL FUNCTION vel(v, a, dt)
		REAL :: v, a, dt

		vel = v + (a * dt)
		RETURN
	END FUNCTION vel

	! Calculates position
	REAL FUNCTION pos(x, v, dt)
		REAL :: x, v, dt

		pos = x + (v * dt)
		RETURN
	END FUNCTION pos
END PROGRAM physics_numerical_euler
