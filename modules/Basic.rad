
// basic definition of printf

print :: (fmt: string, ..) #foreign ;
func  :: () #foreign ;
end   :: () #foreign ;

// Memory functions, these ones come from the CRT
__rad_malloc :: (size: u64) -> * void #foreign ;
__rad_free   :: (ptr: * void) #foreign ;

malloc :: (size: u64) -> * void
{
	return __rad_malloc(size);
}

free :: (ptr: * void) 
{
	__rad_free(ptr);
}

__rad_abort :: () #foreign ;

assert :: (var: bool)
{
	if (!var) __rad_abort();
}
