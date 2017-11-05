

main :: ()
{
//    x := -1;
    x : u64;
    x = add(x, x);
//     printf("X is %lld\n", x);


}

    add := (x: u64, y: u64) -> u64 {return x + y;}


c :: ()
{
   add(0, 0);
}
