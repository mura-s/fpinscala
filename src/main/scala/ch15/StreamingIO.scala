package ch15

import language._

object SimpleStreamTransducers {

  sealed trait Process[I, O] {

    import Process._

    def apply(s: Stream[I]): Stream[O] = this match {
      case Halt() => Stream()
      case Await(recv) => s match {
        case h #:: t => recv(Some(h))(t)
        case xs => recv(None)(xs)
      }
      case Emit(h, t) => h #:: t(s)
    }

    def repeat: Process[I, O] = {
      def go(p: Process[I, O]): Process[I, O] = p match {
        case Halt() => go(this)
        case Await(recv) => Await {
          case None => recv(None)
          case i => go(recv(i))
        }
        case Emit(h, t) => Emit(h, go(t))
      }
      go(this)
    }

    // ex15.5
    def |>[O2](p2: Process[O, O2]): Process[I, O2] = {
      p2 match {
        case Halt() => Halt()
        case Emit(h, t) => Emit(h, this |> t)
        case Await(f) => this match {
          case Emit(h, t) => t |> f(Some(h))
          case Halt() => Halt() |> f(None)
          case Await(g) => Await((i: Option[I]) => g(i) |> p2)
        }
      }
    }

    def map[O2](f: O => O2): Process[I, O2] = this |> lift(f)

    def ++(p: => Process[I, O]): Process[I, O] = this match {
      case Halt() => p
      case Emit(h, t) => Emit(h, t ++ p)
      case Await(recv) => Await(recv andThen (_ ++ p))
    }

    def flatMap[O2](f: O => Process[I, O2]): Process[I, O2] = this match {
      case Halt() => Halt()
      case Emit(h, t) => f(h) ++ t.flatMap(f)
      case Await(recv) => Await(recv andThen (_ flatMap f))
    }

    def zip[O2](p: Process[I, O2]): Process[I, (O, O2)] =
      Process.zip(this, p)

    // ex15.6
    def zipWithIndex: Process[I, (O, Int)] =
      this zip (count map (_ - 1))
  }


  object Process {

    case class Emit[I, O](head: O,
                          tail: Process[I, O] = Halt[I, O]()) extends Process[I, O]

    case class Await[I, O](recv: Option[I] => Process[I, O]) extends Process[I, O]

    case class Halt[I, O]() extends Process[I, O]

    def emit[I, O](head: O,
                   tail: Process[I, O] = Halt[I, O]()): Process[I, O] =
      Emit(head, tail)

    def await[I, O](f: I => Process[I, O],
                    fallback: Process[I, O] = Halt[I, O]()): Process[I, O] =
      Await[I, O] {
        case Some(i) => f(i)
        case None => fallback
      }

    def liftOne[I, O](f: I => O): Process[I, O] =
      Await {
        case Some(i) => Emit(f(i))
        case None => Halt()
      }

    def lift[I, O](f: I => O): Process[I, O] =
      liftOne(f).repeat

    def filter[I](f: I => Boolean): Process[I, I] =
      Await[I, I] {
        case Some(i) if f(i) => emit(i)
        case _ => Halt()
      }.repeat

    def sum: Process[Double, Double] = {
      def go(acc: Double): Process[Double, Double] =
        await(d => emit(d + acc, go(d + acc)))
      go(0.0)
    }

    def id[I]: Process[I, I] = lift(identity)

    // ex15.1
    def take[I](n: Int): Process[I, I] =
      if (n <= 0) Halt()
      else await(i => emit(i, take(n - 1)))

    def drop[I](n: Int): Process[I, I] =
      if (n <= 0) id
      else await(i => drop(n - 1))

    def takeWhile[I](f: I => Boolean): Process[I, I] =
      await(i =>
        if (f(i)) emit(i, takeWhile(f))
        else Halt()
      )

    def dropWhile[I](f: I => Boolean): Process[I, I] =
      await(i =>
        if (f(i)) dropWhile(f)
        else emit(i, id)
      )

    // ex15.2
    def count[I]: Process[I, Int] = {
      def go(n: Int): Process[I, Int] =
        await(i => emit(n + 1, go(n + 1)))
      go(0)
    }

    // ex15.3: pass

    def loop[S, I, O](z: S)(f: (I, S) => (O, S)): Process[I, O] =
      await((i: I) => f(i, z) match {
        case (o, s2) => emit(o, loop(s2)(f))
      })

    // ex15.4
    def sum2: Process[Double, Double] =
      loop(0.0)((d, acc) => (d + acc, d + acc))

    def count2[I]: Process[I, Int] =
      loop(0)((_: I, n) => (n + 1, n + 1))

    import ch11._

    def monad[I]: Monad[({type f[x] = Process[I, x]})#f] =
      new Monad[({type f[x] = Process[I, x]})#f] {
        def unit[O](o: => O): Process[I, O] = emit(o)

        def flatMap[O, O2](p: Process[I, O])(f: O => Process[I, O2]):
        Process[I, O2] = p flatMap f
      }

    def zip[A, B, C](p1: Process[A, B], p2: Process[A, C]): Process[A, (B, C)] =
      (p1, p2) match {
        case (Halt(), _) => Halt()
        case (_, Halt()) => Halt()
        case (Emit(b, t1), Emit(c, t2)) => Emit((b, c), zip(t1, t2))
        case (Await(recv1), _) =>
          Await((oa: Option[A]) => zip(recv1(oa), feed(oa)(p2)))
        case (_, Await(recv2)) =>
          Await((oa: Option[A]) => zip(feed(oa)(p1), recv2(oa)))
      }

    def feed[A, B](oa: Option[A])(p: Process[A, B]): Process[A, B] =
      p match {
        case Halt() => p
        case Emit(h, t) => Emit(h, feed(oa)(t))
        case Await(recv) => recv(oa)
      }

    // ex15.7
    val mean2 = (sum zip count) |> lift { case (s, n) => s / n }

    // ex15.8
    def exists[I](f: I => Boolean): Process[I, Boolean] =
      lift(f) |> any

    def any: Process[Boolean, Boolean] =
      loop(false)((b: Boolean, s) => (s || b, s || b))

    // ex15.9
    def toCelsius(fahrenheit: Double): Double =
      (5.0 / 9.0) * (fahrenheit - 32.0)

    def convertFahrenheit: Process[String, String] =
      filter((line: String) => !line.startsWith("#")) |>
        filter(line => line.trim.nonEmpty) |>
        lift(line => toCelsius(line.toDouble).toString)
  }

  object GeneralizedStreamTransducers {

    trait Process[F[_], O] {

      import Process._

      def onHalt(f: Throwable => Process[F, O]): Process[F, O] = this match {
        case Halt(e) => Try(f(e))
        case Emit(h, t) => Emit(h, t.onHalt(f))
        case Await(req, recv) =>
          Await(req, recv andThen (_.onHalt(f))).asInstanceOf[Process[F, O]]
      }

      def ++(p: => Process[F, O]): Process[F, O] =
        this.onHalt {
          case End => Try(p)
          case err => Halt(err)
        }

      def flatMap[O2](f: O => Process[F, O2]): Process[F, O2] =
        this match {
          case Halt(err) => Halt(err)
          case Emit(o, t) => Try(f(o)) ++ t.flatMap(f)
          case Await(req, recv) =>
            Await(req, recv andThen (_ flatMap f)).asInstanceOf[Process[F, O2]]
        }

    }

    object Process {

      case class Await[F[_], A, O](req: F[A],
                                   recv: Either[Throwable, A] => Process[F, O]) extends Process[F, O]

      case class Emit[F[_], O](head: O,
                               tail: Process[F, O]) extends Process[F, O]

      case class Halt[F[_], O](err: Throwable) extends Process[F, O]

      case object End extends Exception

      case object Kill extends Exception

      def Try[F[_], O](p: Process[F, O]): Process[F, O] =
        try p
        catch {
          case e: Throwable => Halt(e)
        }

      def await[F[_], A, O](req: F[A])
                           (recv: Either[Throwable, A] => Process[F, O]): Process[F, O] =
        Await(req, recv).asInstanceOf[Process[F, O]]
    }

  }

  // ex15.10, 15.11, 15.12: pass

}













