void () main = {
  if (TRUE) {
    doThing();
  }

  if (FALSE) // this always fails
    doThing();

  if (TRUE) /* a */ {
    doThing();
  } else if (FALSE) /* b */ {
    doOtherThing();
  } else /* c */ {
    doNothing();
  }
};