a = 1;
b = 2;

def printa()
{
  print(a);
}

def printk()
{
  print(b);
}

def incab()
{
  a = a + 1;
  b = b + 1;
}

def main()
{
  printa();
  printk();
  incab();
  printa();
  printk();
  return 0;
}
