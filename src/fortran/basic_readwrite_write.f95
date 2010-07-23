! File: src/fortran/basic_readwrite_write.f95

! This program demonstrates how to write data to a file

PROGRAM basic_readwrite_write
	! Open a connection to our file
	OPEN(UNIT=55, FILE="outfile.txt", STATUS="REPLACE")

	WRITE(55,*) "This will be line #1" ! newlines are appended by default
	WRITE(55,*) "This will be line #2"
	WRITE(55,*) "This will be line #3"

	! Close file connection
	! In Fortran, data is not written until CLOSE() or FLUSH() is called
	! (or until the process ends)
	CLOSE(UNIT=55)
END PROGRAM basic_readwrite_write
