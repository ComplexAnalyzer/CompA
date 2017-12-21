def gcd(a, b) {
  while (a != b)
    if (a > b) a = a - b;
    else b = b - a;
  return a;
}

def main()
{
  print(gcd(14,21));
  print(gcd(8,36));
  print(gcd(99,121));
  return 0;
}
