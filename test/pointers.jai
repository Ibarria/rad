#import "Basic"

Vector3 := struct {
    x: float;
    y: float;
    z: float;
}

Apointer: *Vector3;

print_position :: (entity : *Entity) {
    print("Position: (%f, %f, %f)\n", entity.position.x, entity.position.y, entity.position.z);
}

Entity :: struct {
	position: Vector3;
}

main :: ()
{
    myvar: Vector3;
    fx : float;


    myvar.x = 0.0  
    // Silly comment
    myvar.y = 0.0;
    myvar.z = 0.0;

    a_val : s16;

    e : Entity;
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