package ch12

import language._
import ch11._

trait Applicative[F[_]] extends Functor[F] {
  def unit[A](a: => A): F[A]

  def map[A, B](fa: F[A])(f: A => B): F[B] =
    map2(fa, unit(()))((a, _) => f(a))

  def traverse[A, B](as: List[A])(f: A => F[B]): F[List[B]] =
    as.foldRight(unit(List[B]()))((a, fbs) => map2(f(a), fbs)(_ :: _))

  // ex12.1
  def sequence[A](fas: List[F[A]]): F[List[A]] =
    traverse(fas)(fa => fa)

  def replicateM[A](n: Int, fa: F[A]): F[List[A]] =
    sequence(List.fill(n)(fa))

  def product[A, B](fa: F[A], fb: F[B]): F[(A, B)] =
    map2(fa, fb)((_, _))

  // ex12.2
  def apply[A, B](fab: F[A => B])(fa: F[A]): F[B] =
    map2(fab, fa)(_(_))

  def map2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] =
    apply(map(fa)(f.curried): F[B => C])(fb)

  def mapViaApply[A, B](fa: F[A])(f: A => B): F[B] =
    apply(unit(f): F[A => B])(fa)

  // ex12.3
  def map3[A, B, C, D](fa: F[A], fb: F[B], fc: F[C])
                      (f: (A, B, C) => D): F[D] =
    apply(apply(apply(unit(f.curried): F[A => B => C => D])(fa))(fb))(fc)

  def map4[A, B, C, D, E](fa: F[A], fb: F[B], fc: F[C], fd: F[D])
                         (f: (A, B, C, D) => E): F[E] =
    apply(apply(apply(apply(
      unit(f.curried): F[A => B => C => D => E])(fa))(fb))(fc))(fd)

  // ex12.7: pass

  // ex12.8
  //  def product[G[_]](G: Applicative[G]): Applicative[({type f[x] = (F[x], G[x])})#f] = {
  //    val self = this
  //    new Applicative[({type f[x] = (F[x], G[x])})#f] {
  //      def unit[A](a: => A): (F[A], G[A]) = (self.unit(a), G.unit(a))
  //
  //      override def apply[A, B](fs: (F[A => B], G[A => B]))(fg: (F[A], G[A])) =
  //        (self.apply(fs._1)(fg._1), G.apply(fs._2)(fg._2))
  //    }
  //  }

  // ex12.9
  //  def compose[G[_]](G: Applicative[G]): Applicative[({type f[x] = F[G[x]]})#f] = {
  //    val self = this
  //    new Applicative[({type f[x] = F[G[x]]})#f] {
  //      def unit[A](a: => A) = self.unit(G.unit(a))
  //      override def map2[A,B,C](fga: F[G[A]], fgb: F[G[B]])(f: (A,B) => C) =
  //        self.map2(fga, fgb)(G.map2(_,_)(f))
  //    }
  //  }

  // 12.10: pass

  def sequenceMap[K, V](ofa: Map[K, F[V]]): F[Map[K, V]] =
    ofa.foldLeft(unit(Map.empty[K, V])) { case (acc, (k, fv)) =>
      map2(acc, fv)((m, v) => m + (k -> v))
    }

}

object Applicative {
  val streamApplicative = new Applicative[Stream] {
    def unit[A](a: => A): Stream[A] =
      Stream.continually(a)

    override def map2[A, B, C](a: Stream[A], b: Stream[B])(
      f: (A, B) => C): Stream[C] = a zip b map f.tupled

    // ex12.4: pass
  }

  def validationApplicative[E]: Applicative[({type f[x] = Validation[E, x]})#f] =
    new Applicative[({type f[x] = Validation[E, x]})#f] {
      def unit[A](a: => A): Validation[E, A] = Success(a)

      override def map2[A, B, C](fa: Validation[E, A], fb: Validation[E, B])(f: (A, B) => C) =
        (fa, fb) match {
          case (Success(a), Success(b)) => Success(f(a, b))
          case (Failure(h1, t1), Failure(h2, t2)) => Failure(h1, t1 ++ Vector(h2) ++ t2)
          case (e@Failure(_, _), _) => e
          case (_, e@Failure(_, _)) => e
        }
    }
}

trait Monad[F[_]] extends Applicative[F] {
  def flatMap[A, B](ma: F[A])(f: A => F[B]): F[B] =
    join(map(ma)(f))

  override def map[A, B](m: F[A])(f: A => B): F[B] =
    flatMap(m)(a => unit(f(a)))

  override def map2[A, B, C](ma: F[A], mb: F[B])(f: (A, B) => C): F[C] =
    flatMap(ma)(a => map(mb)(b => f(a, b)))

  def compose[A, B, C](f: A => F[B], g: B => F[C]): A => F[C] =
    a => flatMap(f(a))(g)

  def join[A](mma: F[F[A]]): F[A] = flatMap(mma)(ma => ma)

  // ex12.11: pass
}

object Monad {
  def eitherMonad[E]: Monad[({type f[x] = Either[E, x]})#f] =
    new Monad[({type f[x] = Either[E, x]})#f] {
      def unit[A](a: => A): Either[E, A] = Right(a)

      override def flatMap[A, B](eea: Either[E, A])(f: A => Either[E, B]): Either[E, B] =
        eea match {
          case Left(a) => Left(a)
          case Right(a) => f(a)
        }
    }
}

sealed trait Validation[+E, +A]

case class Failure[E](head: E, tail: Vector[E] = Vector()) extends Validation[E, Nothing]

case class Success[A](a: A) extends Validation[Nothing, A]

trait Traverse[F[_]] extends Functor[F] {
  def traverse[G[_] : Applicative, A, B](fa: F[A])(f: A => G[B]): G[F[B]] =
    sequence(map(fa)(f))

  def sequence[G[_] : Applicative, A](fga: F[G[A]]): G[F[A]] =
    traverse(fga)(ga => ga)

  type Id[A] = A

  val idMonad = new Monad[Id] {
    def unit[A](a: => A) = a

    override def flatMap[A, B](a: A)(f: A => B): B = f(a)
  }

  def map[A, B](fa: F[A])(f: A => B): F[B] =
    traverse[Id, A, B](fa)(f)(idMonad)
}

case class Tree[+A](head: A, tail: List[Tree[A]])

object Traverse {
  // ex12.13
  val listTraverse = new Traverse[List] {
    override def traverse[G[_], A, B](as: List[A])(f: A => G[B])(implicit G: Applicative[G]): G[List[B]] =
      as.foldRight(G.unit(List[B]()))((a, fbs) => G.map2(f(a), fbs)(_ :: _))
  }

  val optionTraverse = new Traverse[Option] {
    override def traverse[G[_], A, B](oa: Option[A])(f: A => G[B])(implicit G: Applicative[G]): G[Option[B]] =
      oa match {
        case Some(v) => G.map(f(v))(Some(_))
        case None => G.unit(None)
      }
  }

  val treeTraverse = new Traverse[Tree] {
    override def traverse[G[_], A, B](ta: Tree[A])(f: A => G[B])(implicit G: Applicative[G]): G[Tree[B]] =
      G.map2(f(ta.head), listTraverse.traverse(ta.tail)(tta => traverse(tta)(f)))(Tree(_, _))
  }
}
