def foo(a)
{
  j = 0;
  while (a > 0) {
    j = j + 2;
    a = a - 1;
  }
  return j;
}

def main()
{
  print(foo(7));
  return 0;
}
