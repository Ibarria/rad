
#import "Basic"

var: [10] u32;

/*
  Types of array to support:
  The one with known size:

  var: [10] u32;

  CONST :: 10;
  var: [CONST] u32;

  Without known size

  var: [] u32

  And fully dynamic:

  var: [..] u32;

  Also support array of arrays, arrays in structs,
  and array as part of varReference, array as argument

*/

main :: ()
{



    // printf("X is %llu\n", x1);
    // x = #run (3 + 2);
}

add := (x: u64, y: u64) -> u64 
{
    return x + y;
}

// #run main()


