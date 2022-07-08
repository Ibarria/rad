#if defined(_WINDOWS) || defined(_WIN32)
#define PLATFORM_WINDOWS
#elif defined(__linux__) || defined(__FreeBSD__)
#define PLATFORM_LINUX
#define PLATFORM_POSIX
#elif defined(__APPLE__)
#define PLATFORM_MACOS
#define PLATFORM_POSIX
#endif

extern int main();
extern void end();

#if defined(PLATFORM_LINUX)
  #define ALIGN_PTR __attribute__((force_align_arg_pointer))
#else
  #define ALIGN_PTR
#endif

/*
	Further todo on this section:
	- Have possible global initializers here
	- on termination, flush and close file descriptors
	- Finish temporary files
	- return error code
*/

ALIGN_PTR
void _radstart()
{
	main();
#if defined(PLATFORM_WINDOWS)	
	end();
#elif defined(PLATFORM_LINUX)
    asm("mov $60,%rax;" 
	     "mov $0,%rdi;" 
		 "syscall"
    );
    __builtin_unreachable(); 	
#else
#error "Unimplemented exit"
#endif
}