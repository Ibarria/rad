
#import "Basic"

main :: ()
{
    x1 := 3;
    char_ : u8 = 65;
    sh : s32 = -30000;

    printf("X is %llu, char is %c, sh is %d\n", x1, char_, sh);
    // x = #run (3 + 2);
}

add := (x: u64, y: u64) -> u64 
{
    return x + y;
}

// #run main()


