Thoughts on the C Runtime Library and exit
-------------------------------------------

An early iteration did not have programs built with rad be compiled with the CRT. This caused the program to take a very long time to exit. 
The first solution was to link with the CRT. 
Now, *Basic.rad* defines the function `end`, which calls `ExitProcess` or `_exit` . 
There are a few items to clear here:
* The CRT implements a main that tries to call the actual user defined main. If not using the CRT, the linker needs to know what entrypoint to use. 
* The CRT main, after normal execution, does some cleanup (unless the program called exit). That cleanup involves:
  * Terminating all other threads if there were some
  * Flusing outstanding buffers (think stdout, etc)
  * Closing opened file descriptors
  * call functions defined with _at_exit
  * removing any tmpfile created


Windows `ExitProcess` does handle some of those, on linux is more important to use the CRT (or a rad implementation of such) to keep track of the data to know what to clean. 