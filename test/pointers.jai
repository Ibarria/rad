
#import "Basic"

MyStruct := struct {
    x: f32;
    y: f32;
}

Apointer: *MyStruct;

main :: ()
{
    myvar: MyStruct;
    fx : f32;

    myvar.x = 0.0;
    myvar.y = 0.0;

    Apointer = *myvar;

    fx = Apointer.x;
}

