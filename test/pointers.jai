
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

    Apointer = null;

    if (Apointer == null) print("The pointer is null now\n");
    else print("The pointer is not null now\n");

    Apointer = *myvar;

    if (Apointer == null) print("The pointer is null now\n");
    else print("The pointer is not null now\n");

    fx = Apointer.x;

    print("The value of fx is %f\n", fx);
}

