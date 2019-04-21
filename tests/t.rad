#import "Basic"

test::()
{
	// This is a check of #run calls with result value
	a := #run (30 + 21 + 15);

	print("Inside function, this is called from a GLOBAL #RUN\n\n\n");
// 	#run print("This is directly inside #run\n");

	print("This is a from #run: %d -- \n", a);


	// Test that multiple runs can coexist, for register allocation purposes
	
	b := #run 3 + 2;
	c := #run 31 + 2;
	d := #run 30 + 22;
	e := #run 13 + 2;
    
	print("This is a from #run: [b %d] [c %d] [d %d] [e %d] -- \n", b, c, d, e);


	// Having this instruction will result in an error due to recursive call
	// #run test();
}

main::()
{
	test();
}

#run test();
