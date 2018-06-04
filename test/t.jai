#import "Basic"

test::()
{
	print("Inside function, this is called from a GLOBAL #RUN\n\n\n");
 	#run print("This is directly inside #run\n");

	// This is a check of #run calls with result value
	// a := #run 3 + 2;

	// Having this instruction will result in an error due to recursive call
	// #run test();
}

main::()
{
	test();
}

#run main();
