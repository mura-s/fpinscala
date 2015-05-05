package ch2

// ex2.2
class Sort {
  def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {
    @annotation.tailrec
    def loop(n: Int): Boolean =
      if (n >= as.length) true
      else if (ordered(as(n-1), as(n))) loop(n+1)
      else false

    loop(1)
  }
}
