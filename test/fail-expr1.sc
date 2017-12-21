a = 1;
b = true;

def foo(c, d)    
{
  dd = 11;
  e = false;
  a + c;
  c - a;
  a * 3;
  c / 2;
  d + a; /* Error: bool + int */
}

def main()
{
  foo(2, false);
  return 0;
}
