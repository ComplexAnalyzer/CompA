def foo(a, b)
{
}

def main()
{
  foo(42, true);
  foo(42, 42); /* Fail: int, not bool */
}
