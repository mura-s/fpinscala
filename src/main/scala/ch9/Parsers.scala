package ch9

import language._
import scala.util.matching.Regex

trait Parsers[ParseError, Parser[+ _]] {
  self =>

  def or[A](s1: Parser[A], s2: => Parser[A]): Parser[A]

  implicit def string(s: String): Parser[String]

  implicit def operators[A](p: Parser[A]): ParserOps[A]
  = ParserOps[A](p)

  implicit def asStringParser[A](a: A)(implicit f: A => Parser[String]):
  ParserOps[String] = ParserOps(f(a))

  def map[A, B](a: Parser[A])(f: A => B): Parser[B]

  def char(c: Char): Parser[Char] =
    string(c.toString) map (_.charAt(0))

  def succeed[A](a: A): Parser[A] =
    string("") map (_ => a)

  def slice[A](p: Parser[A]): Parser[String]

  def product[A, B](p: Parser[A], p2: => Parser[B]): Parser[(A, B)]

  // ex9.1
  def map2[A, B, C](p: Parser[A], p2: => Parser[B])(f: (A, B) => C): Parser[C] =
    map(product(p, p2))(f.tupled)

  def many1[A](p: Parser[A]): Parser[List[A]] =
    map2(p, many(p))(_ :: _)

  // ex9.2: pass

  // ex9.3
  def many[A](p: Parser[A]): Parser[List[A]] =
    map2(p, many(p))(_ :: _) or succeed(List())

  // ex9.4
  def listOfN[A](n: Int, p: Parser[A]): Parser[List[A]] =
    if (n <= 0) succeed(List())
    else map2(p, listOfN(n - 1, p))(_ :: _)

  // ex9.5: 以下を使うとmap2を非正格にしなくてもよい
  def wrap[A](p: => Parser[A]): Parser[A]

  def manyViaWrap[A](p: Parser[A]): Parser[List[A]] =
    map2(p, wrap(manyViaWrap(p)))(_ :: _) or succeed(List())

  def flatMap[A,B](p: Parser[A])(f: A => Parser[B]): Parser[B]

  implicit def regex(r: Regex): Parser[String]

  // ex9.6
  for {
    s <- "[0-9]+".r
    i = s.toInt
    _ <- listOfN(i, char('a'))
  } yield i

  // ex9.7
  def productViaFP[A, B](p: Parser[A], p2: => Parser[B]): Parser[(A, B)] =
    for {
      pa <- p
      pb <- p2
    } yield (pa, pb)

  def map2ViaFP[A, B, C](p: Parser[A], p2: => Parser[B])(f: (A, B) => C): Parser[C] =
    for {
      pa <- p
      pb <- p2
    } yield f(pa, pb)

  // ex9.8
  def mapViaFP[A, B](p: Parser[A])(f: A => B): Parser[B] =
    flatMap(p)(a => succeed(f(a)))

  // ex9.10 ~ 9.18: pass

  case class ParserOps[A](p: Parser[A]) {
    def |[B >: A](p2: => Parser[B]): Parser[B] = self.or(p, p2)

    def or[B >: A](p2: => Parser[B]): Parser[B] = self.or(p, p2)

    def map[B](f: A => B): Parser[B] = self.map(p)(f)

    def many = self.many(p)

    def slice: Parser[String] = self.slice(p)

    def **[B](p2: => Parser[B]): Parser[(A, B)] =
      self.product(p, p2)

    def product[B](p2: => Parser[B]): Parser[(A, B)] =
      self.product(p, p2)

    def flatMap[B](f: A => Parser[B]): Parser[B]
      = self.flatMap(p)(f)
  }

}
