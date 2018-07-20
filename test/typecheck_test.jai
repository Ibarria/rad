
#import "Basic"


//global_x1 := #run add(3, 0);

main :: ()
{
//    so: u32;    
//    print("Global_x1 should be 3 and it is: %I64u\n", global_x1);    
//    iden(3);
    fval := fiden(3);
    print("float converted from integer: %f\n", fval);

//    v1 : int = 10;
//    v2 : int = -20;
//    v3 := -32;

    
    // These should be errors
    // ve1 : u32 = -1;
    // ve2 : u16 = 100000;
    // iden(-1);
}

add := (x: u64, y: u64) -> u64 
{
    return x + y;
}

iden :: (a: u32) -> u32
{
    return a;
}

fiden :: (a: f64) -> f64
{
    return a;
}

f :: () -> u16
{
    return 8 + 4 * 12;
}

#run main();