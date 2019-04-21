/*
	This language supports comments and nested comments. 
	
	For imports, you can use #load and #import :
	
	- Import does not use an extension, load does. 
	- Search paths are compiler configured
	- Load is just one more file to be added for the compilation. 
	- Import refers to a library (internal or external), with an associated dll 
	
*/

#import "Basic"

test_arrays :: ()
{
/*
  Types of array to support:
  The one with known size:
*/
	var1: [10] u32;

	CONST :: 10;
	var2: [CONST] u32;
/*
    Possible things to do:
    var.count ->returns 10, might be AST replaced for the literal 10
    When passing an array to a function by value, create a not known size struct
    Is not the same type as the unknown size
    it is an error to try to assign to count
*/
	assert(var1.count == 10);
	assert(var2.count == CONST);

/*
	Without known size
*/
    var3: [] u32
    
	assert(var3.count == 0);
	assert(var3.data == null);

/*	
    It is valid to assign to count and data. The compiler provides functionality for it,
	but it is responsibility of the user to manage these fields
*/
  

/*  
  And fully dynamic:

  var: [..] u32;

  Also support array of arrays, arrays in structs,
  and array as part of varReference, array as argument

  Next step: for static unknown size arrays and dynamic arrays, 
  bytecode needs to get the data pointer instead of just assuming a pointer itself

  After that : Arrays as function parameters. For sized arrays, check the size matters. 
  Allow some implicit conversions? Likely not... dangerous. 
*/

	// a1 is an array of 5 integers, stack allocated
	// The compiler knows the size of the array, it does not convert
	// to pointer or dynamic array. If it did, memory management would be compromised
	a1 := [5] int;
	
	assert(a1.size == 5);
	
	// This will assign 2 to all the elements of the array
	for a1 it = 2;
}

test_var_init :: ()
{
	a : int;
	
	assert(a == 0);
}

/*
	Build can be controlled from the source code itself, and have command line overrides. 
	The compiler has a header that exposes a global structure with:
	- Executable name
	- Search / Include paths
	- Release or debug
	- backend to use for compile (C, LLVM, ?)
	
	The program only needs to do a #run on that function.
*/

/*
	Compiler directives (Replacement for the preprocessor, and extension)
	
	

*/

main :: ()
{
    x1 := 6;
    char_ : u8 = 65;
    sh : s32 = -30000;

    print("X is %lu, char is %c, sh is %d\n", x1, char_, sh);
    func();
    // x = #run (3 + 2);
    print("And now a string before the end\n");
    // end();
}

add := (x: u64, y: u64) -> u64 
{
    return x + y;
}

// #run main()


