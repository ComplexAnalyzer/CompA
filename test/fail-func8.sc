def foo(a, b)
{
}

def bar()
{
}

def main()
{
  foo(42, true);
  foo(42, bar()); /* int and void, not int and bool */
}
