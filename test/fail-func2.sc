def foo(a, b, c) { }

def bar(a, b, a) {} /* Error: duplicate formal a in bar */

def main()
{
  bar(1, 2, 3);
  return 0;
}
