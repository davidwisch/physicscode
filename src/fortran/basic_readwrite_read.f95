! File: src/fortran/basic_readwrite_read.f95

PROGRAM basic_readwrite_read
	IMPLICIT NONE

	REAL, ALLOCATABLE, DIMENSION(:,:) :: X
	INTEGER :: NUM, i
	CHARACTER(1) :: a

	OPEN(UNIT=55, FILE="sampledata.txt", STATUS="OLD")

	READ(55, 10) NUM ! the number of rows of data
	ALLOCATE(X(NUM,5)) ! set our array size to the number of cols, rows

	DO i=1, NUM
		! read the numbers into 'X' and the commas into dummy 'a'
		READ(55, 20) X(i,1), A ,X(i,2), A, X(i,3), A, X(i,4),A, X(i,5)
	END DO

	CLOSE(UNIT=55)

	WRITE(*,*) X(4,3) + X(4,3)

	10 FORMAT(I1)
	20 FORMAT(4(F2.0, A), F2.0)

END PROGRAM basic_readwrite_read
