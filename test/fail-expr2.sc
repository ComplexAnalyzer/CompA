a = 1;
b = 2;

def foo(c, d)    
{
  d = 3;
  e = true;
  b + e; /* Error: bool + int */
}

def main()
{
  foo(1,2);
  return 0;
}
