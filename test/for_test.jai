#import "Basic"

/*
	Test the for functionality:

	Iterating over integers:
	for <start_value> .. <inclusive_end_value>
		<statement block>

	Implicit Iterator name (it), implicit index (it_index), only works for arrays:
	for array
		<statement block>

	Explicit value name, implicit_index (it_index):
	for <value_name> : array 
		<statement block>

	Explicit value and index
	for <value>, <index> : array
		<statement block>

	If the value is declared with a pointer syntax, iterate with pointers (to modify the array):
	for *<value> : array
		<statement block> 


	Besides this functionality, test break, continue, remove (and maybe return)
	Remove as unordered (easier). Assume remove will 'lose' memory, as the count decreases and the array might be sized.
	In this case, remove can only be applied to Sized, Dynamic arrays. 
	Break and continue have an optional parameter, the iterator of the array to refer which array the control flow
	applies to (could be more than one, like an outer loop)
	
*/

SIZE :: 10;

main :: ()
{
	print("For range iteration:\n");
	for 1 .. 5 print("Iterating Range %d\n", it);

	{
		sa : [SIZE] u64;

		print("For basic array iteration:\n");
		for sa sa[it_index] = it_index;

		for sa print("Iterating Basic Array %d\n", it);		
	}

	{
		sa : [SIZE] u64;

		print("For value array iteration:\n");
		for myval : sa sa[it_index] = it_index;

		for myval : sa print("Iterating Value Array %d\n", myval);		
	}

	{
		sa : [SIZE] u64;

		print("For value, index array iteration:\n");
		for myval, myindex: sa sa[myindex] = myindex;

		for myval, myindex: sa print("Iterating Value, Index Array %d\n", myval);		
	}

	// TODO: use/allow pointer syntax on the iterator
	{
		sa : [SIZE] u64;

		print("For value pointer array iteration:\n");
		for *myval: sa <<myval = 523;

		for *myval: sa print("Iterating Value pointer Array %d\n", <<myval);		
	}

    assert(true);
}

#run main();