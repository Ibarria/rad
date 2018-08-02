#import "Basic"

Entity :: struct {
    position: Vector3;
    other: *Entity;
}

Vector3 := struct {
    x: float;
    y: float;
    z: float;
}

main :: ()
{
    arr := new [10] int;
    e := new Entity;
    e.position.x = 10;
//  delete arr;
}