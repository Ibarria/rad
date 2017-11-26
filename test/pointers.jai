
#import "Basic"

MyStruct := struct {
    x: float;
    y: float;
}

Apointer: *MyStruct;

main :: ()
{
    myvar: MyStruct;
    myvar.x = 0.0;
    myvar.y = 0.0;

    Apointer = *myvar;

    fx := Apointer.x;
}

