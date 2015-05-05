package ch2

// ex2.5
class Composer {
  def compose[A,B,C](f: B => C, g: A => B): A => C = {
   a => f(g(a))
  }
}
