package ch2

import org.specs2.mutable._

class FibSpec extends Specification {
  "Fib#fib()" should {
    val f = new Fib()

    "0番目の値は0" in {
      f.fib(0) must_== 0
    }

    "1番目の値は1" in {
      f.fib(1) must_== 1
    }

    "4番目の値は3" in {
      f.fib(4) must_== 3
    }

    "5番目の値は5" in {
      f.fib(5) must_== 5
    }
  }
}
