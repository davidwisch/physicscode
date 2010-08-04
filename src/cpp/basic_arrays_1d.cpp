/*
 * File: src/cpp/basic_arrays_1d.cpp
 * 
 * Demonstrates a 1D array in C++.
 *
 * All C++ demo files with a filename starting with basic_
 * will use the standard namespace.  If you don't know what
 * this means, don't worry, it will be explained in more
 * advanced demos.  For now just include the line:
 * 
 * using namespace std
 *
 * in all of your programs.  The iostream standard library 
 * will also be used for all basic_ programs.  Include the
 * line:
 *
 * #include <iostream>
 *
 * in all of your programs for now.
 */

#include <iostream>

using namespace std;

int main( int argc, char* argv[] )
{
	//These first lines sets the precision of the output,
	//and will be covered in another tutorial.
	cout.setf( ios::fixed, ios::floatfield );
	cout.precision( 1 );

	//Declare an array of length 5 with data of type float
	float data1_Array[ 5 ];

	//Declare and fill an array of length 5 with data of
	// type float.
	float data2_Array[] = { 4.0, 5.0, 6.0, 7.0, 8.0 };

	/*
	 * At this point data1_Array is not initalized.  You 
	 * can not be sure what you will get out of data1_Array. 
	 * Lets set all the values in the array to zero.
	 */

	for( int i = 0;
		 i < sizeof( data1_Array ) / sizeof( data1_Array[0] );
		 i++ )
	{
		data1_Array[ i ] = 0.0;
	}//for loops are the standard way of cycling through an array.

	/* 
	 * The sizeof function ensures we don't overrun the length
	 * of the array and ask for data that doesn't exist. This
	 * isn't strictly necessary and the for loop condition could
	 * have been simply written as:
	 *
	 * for( int i = 0; i < 5; i++)
	 *
	 * Notice that i is less than 5, that's because c++ starts
	 * counting at 0.
	 */

	/*
	 * Now lets print the contents of both arrays to the screen
	 * and copy the contents of one array to the other and vice
	 * versa.  Watch closely, and keep your eye on the queen.
	 */

	for( int i = 0;
		 i < sizeof( data1_Array ) / sizeof( data1_Array[0] ) &&
		 i < sizeof( data2_Array ) / sizeof( data2_Array[0] );
		 i++ )//Notice we stay inside the bounds of both arrays
	{
		float temp = 0;
		cout << "data1_Array element: " << i << " = " 
			 << data1_Array[ i ] << endl;
		cout << "data2_Array element: " << i << " = "
			 << data2_Array[ i ] << endl;

		temp = data1_Array[ i ];
		data1_Array[ i ] = data2_Array[ i ];
		data2_Array[ i ] = temp;
	}//This is a common way to move data around between arrays.

	//Now lets print the two arrays again and see what we get.
	cout << endl << "After the copy the arrays contain: " << endl;
	for( int i = 0;
		 i < sizeof( data1_Array ) / sizeof( data1_Array[0] ) &&
		 i < sizeof( data2_Array ) / sizeof( data2_Array[0] );
		 i++ )
	{
		cout << "data1_Array element: " << i << " = " 
			 << data1_Array[ i ] << endl;
		cout << "data2_Array element: " << i << " = "
			 << data2_Array[ i ] << endl;
	}

	return( 0 );
}
