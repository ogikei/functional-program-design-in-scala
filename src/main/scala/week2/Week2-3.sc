// First evaluated "val x", so z + y + 1 + ....
// After that, "def z" is evaluated because z is called at first,
// so it become 3 + y + 1 + ...
// Finally "lazy val y" is evaluated.
def expr = {
  val x = { print("x"); 1 }
  lazy val y = { print("y"); 2 }
  def z = { print("z"); 3 }
  z + y + x + z + y + x
}
expr
