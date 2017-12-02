
#import "Basic"

//x1 : u8 = 3;
//s := "Hello there";

MyType :: struct {
  x: u32;
  y: string;
}


Out1 :: struct {
  a: string;
  b: Out2;
  c: u8;
}

Out2 :: struct {
  d: u32;
  e: Out3;
  a: u8;
}

Out3 :: struct {
  a1: u8;
  a2: u8;
  a3: u8;
}


main :: ()
{
    so: u32;
    a : Out1;
    a.b.e.a2 = 3;


    x1 := 1;
    x1 = add(3, 0);
//    printf("X is %llu\n", x);

//    x = #run (3 + 2);
}

add := (x: u64, y: u64) -> u64 
{
    return x + y;
}

// #run main()