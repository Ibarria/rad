
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
  a: u8;
}

test_pointer :: ()
{
//    m : *MyType;
//    here : MyType;
}


global_x1 := #run add(3, 0);

main :: ()
{

    so: u32;
    a : Out1;
    mt : MyType;
    a.b.d = 3;
    mt.x = 237;
    
    print("Global_x1 should be 3 and it is: %I64u\n", global_x1);
    // x1 := add(3, 0);
    print("a.b.d is %u, mt.x: %d, add 3+2 = %I64u\n", a.b.d, mt.x, add(3, 2));
    
    iden(3);
}

add := (x: u64, y: u64) -> u64 
{
    return x + y;
}

iden :: (a: u32) -> u32
{
    return a;
}

#run main();