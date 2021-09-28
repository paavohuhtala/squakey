float () test = {
  switch (x) {
    case 1: return 1;

    case 2: case 3:
      break;

    default:
      return 3;
  }

  return 2;
};