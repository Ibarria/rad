
#import "Basic"

main :: ()
{
    x1 := 6;
    char_ : u8 = 65;
    sh : s32 = -30000;

    print("X is %lu, char is %c, sh is %d\n", x1, char_, sh);
    // x = #run (3 + 2);
}

add := (x: u64, y: u64) -> u64 
{
    return x + y;
}

// #run main()


