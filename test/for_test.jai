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

main :: ()
{
	for 1 .. 5 print("Iterating %d\n", it);
}