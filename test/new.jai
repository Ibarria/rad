#import "Basic"

Entity :: struct {
    position: Vector3;
    other: *Entity;
    num: u32;
}

Vector3 := struct {
    x: float;
    y: float;
    z: float;
}

main :: ()
{
    // Arrays are not called with new, this is just an array pointer
//    arr := new [10] int;

    e := new Entity;
    print("Address of E: %p\n", e);
    e.position.x = 10;
    e.num = 237;
    print("E position is %f\n", e.position.x);
    print("E num is %d\n", e.num);
//  delete arr;
}

#run main();