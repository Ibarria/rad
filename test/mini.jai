
#import "modules/basic.jai"

main :: ()
{
//    x := -1;
    x : u64;
//    x = add(3, 0);
//    printf("X is %llu\n", x);

    x = #run (3 + 2);
}

add := (x: u64, y: u64) -> u64 {return x + y;}
