package ch4

import org.specs2.mutable._

class OptionSpec extends Specification {
  "ex4.1 Option" should {
    "#map()" in {
      Some(10).map(_ * 2) must_== Some(20)
      (None:Option[Int]).map(_ * 2) must_== None
    }

    "#getOrElse" in {
      Some(10).getOrElse(0) must_== 10
      (None:Option[Int]).getOrElse(0) must_== 0
    }

    "#flatMap()" in {
      Some(10).flatMap(a => Some(a * 2)) must_== Some(20)
    }

    "#orElse()" in {
      (None:Option[Int]).orElse(Some(0)) must_== Some(0)
    }

    "#filter()" in {
      Some(10).filter(_ > 5) must_== Some(10)
      Some(10).filter(_ > 10) must_== None
    }
  }

  "ex4.2 Option#variance()" should {
    "分散を返す" in {
      Option.variance(Seq(2.0, 2.0, 2.0)) must_== Some(0.0)
      Option.variance(Seq()) must_== None
    }
  }

  "ex4.3 Option#map2()" should {
    "Option型の2つの値を結合する" in {
      Option.map2(Some(10), Some(10))(_ + _) must_== Some(20)
      Option.map2(Some(10), None)(_ + _) must_== None
    }
  }

  "ex4.4 Option#sequence()" should {
    "List[Option[Int]]がOption[List[Int]]に変換される" in {
      Option.sequence(List(Some(1), Some(2), Some(3))) must_== Some(List(1,2,3))
    }

    "Noneを含んでいる場合はNoneになる" in {
      Option.sequence(List(Some(1), None, Some(3))) must_== None
    }
  }

  "ex4.5 Option#traverse()" should {
    "a: List(1,2,3), f: Int => Option[Int]のケース" in {
      Option.traverse(List(1,2,3))(a => Some(a * 2)) must_== Some(List(2,4,6))
    }

    "ListにNoneを含むケース" in {
      Option.traverse(List(Some(1), None, Some(3)))(o => o) must_== None
    }
  }
}
