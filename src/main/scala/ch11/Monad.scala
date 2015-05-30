package ch11

import language._
import ch7._
import ch7.Par._
import ch8._
import ch9._

trait Functor[F[_]] {
  def map[A, B](fa: F[A])(f: A => B): F[B]

  def distribute[A, B](fab: F[(A, B)]): (F[A], F[B]) =
    (map(fab)(_._1), map(fab)(_._2))

  def codistribute[A, B](e: Either[F[A], F[B]]): F[Either[A, B]] = e match {
    case Left(fa) => map(fa)(Left(_))
    case Right(fb) => map(fb)(Right(_))
  }
}

object Functor {
  var listFunctor = new Functor[List] {
    def map[A, B](as: List[A])(f: A => B): List[B] = as map f
  }
}

trait Monad[F[_]] extends Functor[F] {
  def unit[A](a: => A): F[A]

  def flatMap[A, B](ma: F[A])(f: A => F[B]): F[B]

  def map[A, B](ma: F[A])(f: A => B): F[B] =
    flatMap(ma)(a => unit(f(a)))

  def map2[A, B, C](ma: F[A], mb: F[B])(f: (A, B) => C): F[C] =
    flatMap(ma)(a => map(mb)(b => f(a, b)))

  // ex11.3
  def sequence[A](lma: List[F[A]]): F[List[A]] =
    lma.foldRight(unit(List[A]()))((ma, lmb) => map2(ma, lmb)(_ :: _))

  def traverse[A, B](la: List[A])(f: A => F[B]): F[List[B]] =
    sequence(la.map(f))

  // ex11.4
  def replicateM[A](n: Int, ma: F[A]): F[List[A]] =
    sequence(List.fill(n)(ma))

  // ex11.5: pass

  def product[A, B](ma: F[A], mb: F[B]): F[(A, B)] = map2(ma, mb)((_, _))

  // ex11.6
  def filterM[A](ms: List[A])(f: A => F[Boolean]): F[List[A]] =
    ms match {
      case Nil => unit(Nil)
      case h :: t => flatMap(f(h))(b =>
        if (!b) filterM(t)(f)
        else map(filterM(t)(f))(h :: _))
    }
}

object Monod {
  val genMonad = new Monad[Gen] {
    def unit[A](a: => A): Gen[A] = Gen.unit(a)

    def flatMap[A, B](ma: Gen[A])(f: A => Gen[B]): Gen[B] = ma flatMap f
  }

  // ex11.1
  val parMonad = new Monad[Par] {
    def unit[A](a: => A): Par[A] = Par.unit(a)

    def flatMap[A, B](ma: Par[A])(f: A => Par[B]): Par[B] = Par.flatMap(ma)(f)
  }

  def parserMonad[E, P[+ _]](p: Parsers[E, P]) = new Monad[P] {
    def unit[A](a: => A) = p.succeed(a)

    def flatMap[A, B](ma: P[A])(f: A => P[B]): P[B] = p.flatMap(ma)(f)
  }

  val optionMonad = new Monad[Option] {
    def unit[A](a: => A) = Some(a)

    def flatMap[A, B](ma: Option[A])(f: A => Option[B]): Option[B] = ma flatMap f
  }

  val streamMonad = new Monad[Stream] {
    def unit[A](a: => A) = Stream(a)

    def flatMap[A, B](ma: Stream[A])(f: A => Stream[B]): Stream[B] = ma flatMap f
  }

  val listMonad = new Monad[List] {
    def unit[A](a: => A) = List(a)

    def flatMap[A, B](ma: List[A])(f: A => List[B]): List[B] = ma flatMap f
  }

  // ex11.2: pass

}