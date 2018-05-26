#import "Basic"

test_param ::(n: u64)
{
	print("This is n: %d\n", n);
}


factorial :: (n: u64) -> u64
{
	if (n <= 1) return 1;
	return n * factorial(n-1);
}


main::()
{
	n : u64 = 3;
	arg : u64 = 10;
	bb := n < arg;
	u :u64;

	test_param(3);

	if (bb) print("Yeah true\n");

	u = factorial(10);

	print("Factorial of 10 is %d\n", u);
	
}