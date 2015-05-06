package ch4

sealed trait Either[+E, +A] {
  // ex4.6
  def map[B](f: A => B): Either[E, B] =
    this match {
      case Left(v) => Left(v)
      case Right(v) => Right(f(v))
    }

  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] =
    this match {
      case Left(v) => Left(v)
      case Right(v) => f(v)
    }

  def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] =
    this match {
      case Left(_) => b
      case Right(v) => Right(v)
    }

  def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] =
    flatMap(aa => b.map(bb => f(aa, bb)))
    // or for { a <- this; b1 <- b } yield f(a,b1)
}

case class Left[+E](value: E) extends Either[E, Nothing]
case class Right[+A](value: A) extends Either[Nothing, A]

object Either {
  def Try[A](a: => A): Either[Exception, A] =
    try Right(a)
    catch { case e: Exception => Left(e) }

  // ex4.7
  def sequence[E, A](es: List[Either[E, A]]): Either[E, List[A]] =
    es match {
      case Nil => Right(Nil)
      case h :: t => h flatMap (hh => sequence(t) map (hh :: _))
    }

  def traverse[E, A, B](as: List[A])(f: A => Either[E, B]): Either[E, List[B]] =
    as match {
      case Nil => Right(Nil)
      case h::t => f(h).map2(traverse(t)(f))(_ :: _)
    }
}

// ex4.8 pass
