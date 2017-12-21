def cond(b)
{
  if (b)
    x = 42;
  else
    x = 17;
  return x;
}

def main()
{
 print(cond(true));
 print(cond(false));
 return 0;
}
