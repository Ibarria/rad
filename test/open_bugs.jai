#import "Basic"

// This line here fails because now we process global variables
// before we do any #run work, so #run can reference global vars.
// A possible solution is to execute run before globals, but what
// Would happen with consts, or global consts?
global_x1 := #run add(3, 0);

add := (x: u64, y: u64) -> u64 
{
    return x + y;
}

main :: ()
{
	// This fails because we try to do llvm initialization when we 
	// declare the variable, but for some reason, the function call is issued
	// after the variable declaration section (llvm allows stores in there, and that creates a backwards cycle)
	x1 := add (2, 1);
}