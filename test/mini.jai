
#import "modules/basic.jai"

x1 : float = .000257;
//s := "Hello there";

a : bool;

main :: ()
{
    fl : s32 = 32000000;
    x1 := 1;
    x1 = add(3, 0);

    a = x1 < 4;
//    printf("X is %llu\n", x);

//    x = #run (3 + 2);
}

add := (x: u64, y: u64) -> u64 
{
    return x + y;
}

// #run main()