
#import "Basic"

MyStruct := struct {
    x: float;
    y: float;
}

Apointer: *MyStruct;

main :: ()
{
    myvar: MyStruct;
    fx : float;

    myvar.x = 0.0;
    myvar.y = 0.0;

    a_val : s16;

    Apointer = null;

    check_pointer(null);
    check_pointer(*a_val);


    if (Apointer == null) print("The pointer is null now\n");
    else print("The pointer is not null now\n");

    Apointer = *myvar;

    if (Apointer == null) print("The pointer is null now\n");
    else print("The pointer is not null now\n");

    fx = Apointer.x;

    print("The value of fx is %f\n", fx);
}

check_pointer :: (p: *s16)
{
    if (p == null) print("check_pointer: the pointer is null now\n");
    else print("check_pointer: the pointer is not null now\n");	
}

#run main();