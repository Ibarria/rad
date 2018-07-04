
// basic definition of printf

print :: (fmt: string, ..) #foreign ;
func  :: () #foreign ;
end   :: () #foreign ;

// Memory functions, these ones come from the CRT
_malloc :: (size: u64) -> * void #foreign ;
_free   :: (ptr: * void) #foreign ;

malloc :: (size: u64) -> * void
{
	return _malloc(size);
}

free :: (ptr: * void) 
{
	_free(ptr);
}
