def main()
{
  i = 1;

  while (i != 5) {
    i = i + 1;
  }

  while (true) {
    foo(); /* foo undefined */
  }

}
