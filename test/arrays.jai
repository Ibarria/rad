
#import "Basic"

var: [] u32;

/*
  Types of array to support:
  The one with known size:

  var: [10] u32;

  CONST :: 10;
  var: [CONST] u32;

    Possible things to do:
    var.count ->returns 10, might be AST replaced for the literal 10
    When passing an array to a function by value, create a not known size struct
    Is not the same type as the unknown size
    it is an error to try to assign to count



  Without known size

  var: [] u32
    
    It is valid to assign to count and data
  
  
  
  
  And fully dynamic:

  var: [..] u32;

  Also support array of arrays, arrays in structs,
  and array as part of varReference, array as argument

  Next step: for static unknown size arrays and dynamic arrays, 
  bytecode needs to get the data pointer instead of just assuming a pointer itself

  After that : Arrays as function parameters. For sized arrays, check the size matters. 
  Allow some implicit conversions? Likely not... dangerous. 
  
*/

main :: ()
{
    x:= var[5];

    var[2] = x;

    // printf("X is %llu\n", x1);
    // x = #run (3 + 2);
}

add := (x: u64, y: u64) -> u64 
{
    return x + y;
}

// #run main()


