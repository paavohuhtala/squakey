float() test =
{
  switch (x)
  {
    // A!
    case 1:
      return 1;

      // B!
    case 2:
    case 3:
      break;

    default: // Hello!
      return 3;
  }

  return 2;
};
