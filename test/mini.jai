
#import "modules/basic.jai"

x1 : u8 = 3;
s := "Hello there";

MyType :: struct {
  x: float;
  y: string;
}

main :: ()
{
    a : MyType;
    a.x = 3.0;


    x1 := 1;
    x1 = add(3, 0);
//    printf("X is %llu\n", x);

//    x = #run (3 + 2);
}

add := (x: u64, y: u64) -> u64 
{
    return x + y;
}

#run main()