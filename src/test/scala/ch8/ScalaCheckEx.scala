package ch8

import org.scalacheck.Properties
import org.scalacheck.Prop._

object ScalaCheckEx extends Properties("String") {
  // sample
  property("startsWith") = forAll { (a: String, b: String) =>
    (a+b).startsWith(a)
  }

  // ex8.1
  property("sum") = forAll { (l: List[Int]) =>
    l.sum == l.reverse.sum
  }

  // ex8.2
  property("max of list") = forAll { (l: List[Int]) =>
    l match {
      case (_ :: _) => l.max == l.sorted.reverse.head
      case _ => throws(classOf[UnsupportedOperationException])(l.max)
    }
  }

}