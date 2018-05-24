#import "Basic"

sum :: (a: int, b: float) -> int{
	return a;
}

main::()
{
// 	A := 42;
	bb : bool = true;

//	B := 3.14;
//	C := sum(A, B);
	if (bb) {
		print("Inside the first if\n");
		if (!bb) print("No bb, first if\n");
		else print("First If, but bb\n");
		print("Outside the first if, before the else\n");
	}
	else print("Main second else\n");
}