package ch2

// ex2.4
class UnCurry {
  def uncurry[A,B,C](f: A => B => C): (A, B) => C = {
    (a, b) => f(a)(b)
  }
}
