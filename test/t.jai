#import "Basic"

test::()
{
	print("Inside function, this is called from a GLOBAL #RUN\n\n\n");
	#run print("This is directly inside #run\n");
	// This will be a check of recursive #run calls
	// a := #run 3 + 2;
}

main::()
{
	test();
}

#run main();