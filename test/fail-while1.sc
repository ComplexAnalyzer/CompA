def main()
{
  i = 1;

  while (i != 5) {
    i = i + 1;
  }

  while (42) { /* Should be boolean */
    i = i + 1;
  }

}
