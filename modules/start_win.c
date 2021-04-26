
extern void main();
extern void end();

/*
	Further todo on this section:
	- Have possible global initializers here
	- on termination, flush and close file descriptors
	- Finish temporary files
	- return error code
*/

void _start()
{
	main();
	end();
}