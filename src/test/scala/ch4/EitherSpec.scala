package ch4

import org.specs2.mutable._

class EitherSpec extends Specification {
  "ex4.6 Either#map()" should {
    "RightのintをStringに変換" in {
      Right(10).map(_.toString()) must_== Right("10")
    }

    "Left値が存在するケース" in {
      Left(-1).map(_.toString()) must_== Left(-1)
    }
  }

  "ex4.6 Either#flatMap()" should {
    "IntをEither[E, String]に変換する関数を渡す" in {
      Right(10).flatMap(v => Right(v.toString())) must_== Right("10")
    }
  }

  "ex4.6 Either#orElse()" should {
    "Left値が存在するケース" in {
      Left(-1).orElse(Right(1)) must_== Right(1)
    }
  }

  "ex4.6 Either#map2()" should {
    "両方のEitherにRight値が存在するケース" in {
      Right(1).map2(Right(2))(_ + _) must_== Right(3)
    }

    "Left値が存在するケース" in {
      Left(-1).map2(Left(-2))((a,b) => b) must_== Left(-1)
    }
  }

  "ex4.7 Either#sequence()" should {
    "全てRightの要素を持つList" in {
      Either.sequence(List(Right(1), Right(2), Right(3))) must_== Right(List(1,2,3))
    }
  }

  "ex4.7 Either#traverse()" should {
    "List[Int]をEither[E, List[Int]]に変換" in {
      Either.traverse(List(1,2,3))(a => Right(a * 2)) must_== Right(List(2,4,6))
    }
  }
}
