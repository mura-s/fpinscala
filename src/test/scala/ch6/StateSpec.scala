package ch6

import org.specs2.mutable._

class StateSpec extends Specification {
  "SimpleRNG#nextInt()" should {
    "seedが42の場合" in {
      SimpleRNG(42).nextInt must_== (16159453, SimpleRNG(1059025964525L))
    }
  }

  "RNG#nonNegativeInt()" should {
    "seedが42の場合" in {
      val rng = SimpleRNG(42)
      val (i1, r1) = RNG.nonNegativeInt(rng)
      // nextIntがプラスの値を返すケース
      (i1, r1) must_== (16159453, SimpleRNG(1059025964525L))

      // nextIntがマイナスの値を返すケース
      RNG.nonNegativeInt(r1) must_== (1281479696,SimpleRNG(197491923327988L))
    }
  }

  "RNG#double()" should {
    "seedが42の場合" in {
      val (i, _) = RNG.double(SimpleRNG(42))
      i must beLessThan(1.0)
      i must beGreaterThanOrEqualTo(0.0)
    }
  }

  // ex6.3: pass

  "RNG#ints()" should {
    "countが3のケース" in {
      RNG.ints(3)(SimpleRNG(42))._1 must have size(3)
    }
  }

  "RNG#doubleViaMap()" should {
    "seedが42の場合" in {
      val (i, _) = RNG.doubleViaMap(SimpleRNG(42))
      i must beLessThan(1.0)
      i must beGreaterThanOrEqualTo(0.0)
    }
  }

  // ex6.6: pass

  // ex6.7
  "RNG#ints()" should {
    "countが3のケース" in {
      RNG.intsViaSeq(3)(SimpleRNG(42))._1 must have size(3)
    }
  }

  // ex6.8, 6.9, 6.10: pass
}
