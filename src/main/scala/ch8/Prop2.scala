package ch8

import ch7.Par.Par
import ch7._
import ch8.Prop2._
import ch6._
import ch5._
import Gen._
import java.util.concurrent.Executors

object Prop2 {
  type FailedCase = String
  type SuccessCount = Int
  type TestCases = Int
  type MaxSize = Int

  sealed trait Result {
    def isFalsified: Boolean
  }

  case object Passed extends Result {
    def isFalsified = false
  }

  case class Falsified(failure: FailedCase,
                       successes: SuccessCount) extends Result {
    def isFalsified = true
  }

  def forAll[A](as: Gen[A])(f: A => Boolean): Prop2 = Prop2 {
    (n, rng) => randomStream(as)(rng).zip(Stream.from(0)).take(n).map {
      case (a, i) => try {
        if (f(a)) Passed else Falsified(a.toString, i)
      } catch {
        case e: Exception => Falsified(buildMsg(a, e), i)
      }
    }.find(_.isFalsified).getOrElse(Passed)
  }

  def apply(f: (TestCases, RNG) => Result): Prop2 =
    Prop2 { (_, n, rng) => f(n, rng) }

  def forAll[A](g: SGen[A])(f: A => Boolean): Prop2 =
    forAll(g(_))(f)

  def forAll[A](g: Int => Gen[A])(f: A => Boolean): Prop2 = Prop2 {
    (max, n, rng) =>
      val casesPerSize = (n - 1) / max + 1
      val props: Stream[Prop2] =
        Stream.from(0).take((n min max) + 1).map(i => forAll(g(i))(f))
      val prop: Prop2 =
        props.map(p => Prop2 { (max, n, rng) =>
          p.run(max, casesPerSize, rng)
        }).toList.reduce(_ && _)
      prop.run(max, n, rng)
  }


  def randomStream[A](g: Gen[A])(rng: RNG): Stream[A] =
    Stream.unfold(rng)(rng => Some(g.sample.run(rng)))

  def buildMsg[A](s: A, e: Exception): String =
    s"test case: $s\n" +
      s"generated an exception: ${e.getMessage}\n" +
      s"stack trace:\n ${e.getStackTrace.mkString("\n")}"

  val S = weighted(
    choose(1, 4).map(Executors.newFixedThreadPool) -> .75,
    unit(Executors.newCachedThreadPool) -> .25)

  def forAllPar[A](g: Gen[A])(f: A => Par[Boolean]): Prop2 =
    forAll(S.map2(g)((_, _))) { case (s, a) => f(a)(s).get }

  def equal[A](p: Par[A], p2: Par[A]): Par[Boolean] =
    Par.map2(p,p2)(_ == _)

  val pint = Gen.choose(0, 10) map (Par.unit(_))
  val p4 = forAllPar(pint)(n => equal(Par.map(n)(y => y), n))

  // ex8.17
  val forkProp = forAllPar(pint)(n => equal(Par.fork(n), n))

  // ex8.18
  // l.takeWhile(f) ++ l.dropWhile(f) == l

  // ex8.19: 引数のhashcodeとRNGを使ってInt値を返す。実装はpass

  // ex8.20: ex8.14などの使い方を参照
}

//trait Prop2 {
//  def check: Either[(FailedCase, SuccessCount), SuccessCount]
//}

case class Prop2(run: (MaxSize, TestCases, RNG) => Result) {
  // ex8.9
  def &&(p: Prop2): Prop2 = Prop2 {
    (m, n, rng) => run(m, n, rng) match {
      case Passed => p.run(m, n, rng)
      case x => x
    }
  }

  def ||(p: Prop2): Prop2 = Prop2 {
    (m, n, rng) => run(m, n, rng) match {
      case Falsified(f, _) => p.tag(f).run(m, n, rng)
      case x => x
    }
  }

  def tag(msg: String): Prop2 = Prop2 {
    (m, n, rng) => run(m, n, rng) match {
      case Falsified(f, s) => Falsified(msg + "¥n" + f, s)
      case x => x
    }
  }
}

case class Gen[+A](sample: State[RNG, A]) {
  def map[B](f: A => B): Gen[B] =
    Gen(sample.map(f))

  def map2[B, C](g: Gen[B])(f: (A, B) => C): Gen[C] =
    Gen(sample.map2(g.sample)(f))

  // ex8.6
  def flatMap[B](f: A => Gen[B]): Gen[B] =
    Gen(sample.flatMap(a => f(a).sample))

  def listOfN(size: Gen[Int]): Gen[List[A]] =
    size.flatMap(n => Gen.listOfN(n, this))

  def listOfN2(size: Int): Gen[List[A]] =
    Gen.listOfN(size, this)

  // ex8.10
  def unsized: SGen[A] = SGen(_ => this)
}

object Gen {
  // ex8.4
  def choose(start: Int, stopExclusive: Int): Gen[Int] =
    Gen(State(RNG.nonNegativeInt).map(i => start + i % (stopExclusive - start)))

  // ex8.5
  def unit[A](a: => A): Gen[A] =
    Gen(State.unit(a))

  def boolean: Gen[Boolean] =
    Gen(State(RNG.boolean))

  def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] =
    Gen(State.sequence(List.fill(n)(g.sample)))

  // ex8.7
  def union[A](g1: Gen[A], g2: Gen[A]): Gen[A] =
    boolean flatMap (b => if (b) g1 else g2)

  // ex8.8
  def weighted[A](g1: (Gen[A], Double), g2: (Gen[A], Double)): Gen[A] = {
    val g1Th = g1._2.abs / (g1._2.abs + g2._2.abs)
    Gen(State(RNG.double).flatMap {
      d => if (d < g1Th) g1._1.sample else g2._1.sample
    })
  }

  // ex8.12
  def listOf[A](g: Gen[A]): SGen[List[A]] =
    SGen(i => g.listOfN2(i))

  // ex8.13
  def listOf1[A](g: Gen[A]): SGen[List[A]] =
    SGen(i => g.listOfN2(i max 1))

  val smallInt = Gen.choose(-10, 10)
  val maxProp = forAll(listOf1(smallInt)) { ns =>
    val max = ns.max
    !ns.exists(_ > max)
  }

  // ex8.14
  val sortProp = forAll(listOf1(smallInt)) { l =>
    val sorted = l.sorted
    sorted.isEmpty || sorted.tail.isEmpty ||
      !sorted.zip(sorted.tail).exists { case (a, b) => a > b }
  }

  // ex8.15: pass

  // ex8.16: pass

}

case class SGen[+A](forSize: Int => Gen[A]) {
  // ex8.11
  def flatMap[B](f: A => Gen[B]): SGen[B] =
    SGen(forSize andThen (_ flatMap f))

  def apply(n: Int): Gen[A] = forSize(n)
}
