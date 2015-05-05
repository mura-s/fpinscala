package ch2

// ex2.3
class Curry {
  def curry[A,B,C](f: (A, B) => C): A => (B => C) = {
    a => b => f(a, b)
  }
}
