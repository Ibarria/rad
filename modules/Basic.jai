
// basic definition of printf

print :: (fmt: string, ..) #foreign ;
func  :: () #foreign ;
end   :: () #foreign ;

// Memory functions, these ones come from the CRT
malloc :: (size: u64) -> * void #foreign ;
free   :: (ptr: * void) #foreign ;
