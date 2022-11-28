void() foo =
{
  if (self.wait > 0)
  {
    self.think = multi_wait;
    self.nextthink = time + self.wait;
  }
  else
  {
    // we can't just remove (self) here, because this is a touch function
    // called wheil C code is looping through area links...
    self.touch = SUB_Null;
    self.nextthink = time + 0.1;
    self.think = SUB_Remove;
  }
};
