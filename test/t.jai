#import "Basic"

test::()
{
	print("Inside function\n");
	// This will be a check of recursive #run calls
	// a := #run 3 + 2;
}

main::()
{
	#run test();
}