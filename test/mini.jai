
#import "Basic"

main :: ()
{
    x1 := 3;

    printf("X is %llu\n", x1);
    // x = #run (3 + 2);
}

add := (x: u64, y: u64) -> u64 
{
    return x + y;
}

// #run main()


