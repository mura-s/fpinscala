package ch2

import org.specs2.mutable._

class SortSpec extends Specification {
  "Sort#isSorted()" should {
    val s = new Sort()

    "Int型でsortされているケース" in {
       s.isSorted(Array(1, 2, 3), (x: Int, y: Int) => x <= y) must_== true
    }

    "Int型でsortされていないケース" in {
      s.isSorted(Array(1, 3, 2), (x: Int, y: Int) => x <= y) must_== false
    }

    "Stringの文字列の長さ順でsortされているケース" in {
      s.isSorted(Array("a", "aa", "aaa"),
        (a: String, b: String) => a.length <= b.length) must_== true
    }

    "Stringの文字列の長さ順でsortされていないケース" in {
      s.isSorted(Array("a", "aaaa", "aaa"),
        (a: String, b: String) => a.length <= b.length) must_== false
    }
  }
}
