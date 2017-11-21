Introduction
============

This code is an effort to write a compiler for a new programming language. I am drawing inspiration from JAI from Jonathan Blow, a bit from Go, and a bit from Swift.

This language does not have a name yet, I have toyed with `c2` , `jai`, `jet`.

The language is statically typed, with option to runtime information and compile time execution. In the future the language will compile to LLVM, but for now it compiles to C. 

Declaring Variables
-------------------

Variables are declared with the name on the left first, then type, and then initial value. For example:

```cpp
x : u64 = 3;
```

Variables can also be implicit (only the initial value):

```cpp
x := 3;
```

Or a variable could have no initial value:

```cpp
x : u64 ;
```

This extends to functions, which are variable types:

```cpp
add := (x: float, y: float) -> float 
{
    return x + y;
}
```

Function pointers are just the same, but this time in type form:

```cpp
add : (x: float, y: float) -> float;
```

Thoughts on Arrays
------------------

We support 3 types of arrays:
 1) Very C like array, size known at compile time:
    array_1 := [50] u16;
	
 2) Static array, where all elements have a valid value
    array_2 := [] u16;
	array_2[30]; => access element 30. Bounds check possible
	array_2.data => this is a ^u16
	array_2.count => number of elements in the array
	Both data and count are visible and can be assigned and modified (at your peril)

 3) Dynamic array, size can change and not all elements might be used
    array_3 := [..] u16;
	array_3[30]; => access element 30. Bounds check possible
	array_3.data => this is a ^u16
	array_3.size => number of elements in the array total
	array_3.count => number of used, valid elements in the array
	
 How do these arrays convert to each other? 
  - A Dynamic array can convert to a static array, by losing information? NO
    This has the risk that if the conversion is on a function that tries to access internals,
	it could break since internals differ. 
  - A C like array converts to static array?
    Playing with internals is again a risk, but less so than in the previous example
	This would force C arrays to store the size, when in most cases it would be enough to just 
	use the size from the AST. Maybe provide the size if the array is ever passed in a function. 


Thoughts on RUN directive
-------------------------

The RUN directive is a powerful feature of the language, but I am wrestling on how it will be implemented. All the code will be compiled to bytecode (as possible, as run directives might be needed). Then, RUN directives can be used to trigger a function call (in the correct scope), referencing only global variables, external functions or literals. This is to bound the dependencies for a run call. 
A RUN directive can not produce any output, or produce code that later is inserted into the compilation step. 