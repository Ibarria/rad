
#import "Basic"

// var: [10] u32;

/*
  Types of array to support:

  Static Array:

  var: [10] u32;

  CONST :: 10;
  var: [CONST] u32;

    Possible things to do:
    var.count ->returns 10, might be AST replaced for the literal 10
    When passing an array to a function by value, create a not known size struct
    Is not the same type as the unknown size
    it is an error to try to assign to count
    This type is implemented as a plain C array (in bytecode, in llvm)


  Sized arrays:

  var: [] u32
    
    It is valid to assign to count and data
    This type can only be declared as a function parameter, not as a local variable.
    The idea is to force local variables to be fully dynamic arrays, while this
    type is the common denominator between static and dynamic arrays.
    The user can access count, and access members. Treat count as const. 
    
  
  And fully dynamic:

  var: [..] u32;

  Also support array of arrays, arrays in structs,
  and array as part of varReference, array as argument

  This type has:
    .data : data pointer, used for RT functions to grow the array
    .count : total number of elements in the array (actually used)
    .reserved_size : number of slots in data, used or not

  Assigning to a number less than count is an error (if checks are enabled), otherwise
  use the array_add function which will do the grow if needed be. 

  Next step: for static unknown size arrays and dynamic arrays, 
  bytecode needs to get the data pointer instead of just assuming a pointer itself

  After that : Arrays as function parameters. For sized arrays, check the size matters. 
  Allow some implicit conversions? Likely not... dangerous. 
  
*/

test_static_array :: ()
{
    
    inner : [5] u32;

    inner[0] = 0;
 
    
    inner[1] = 1;
    inner[2] = 2;
    inner[3] = 3;
    inner[4] = inner[2];

    print("Inner is a static array with %d elements\n", inner.count);
    print("inner values are %d %d %d %d %d\n", inner[0], inner[1], inner[2], inner[3], inner[4]);


// This should be an error, only declare this as parameters
// err : [] int;
}



main :: ()
{
    test_static_array();
/*    
    inner[1] = 1;
    inner[2] = 2;
    inner[3] = 3;

    print("Inner is a static array with %d elements\n", inner.count);

    other : u32;
    other = inner[3];
    x:= var[5];

    var[2] = x;

    print("inner values are %d %d %d %d\n", inner[0], inner[1], inner[2], inner[3]);
    print("X is %d\n", x);
*/
    // printf("X is %llu\n", x1);
}

/*
add := (x: u64, y: u64) -> u64 
{
    return x + y;
}
*/
// #run main()


