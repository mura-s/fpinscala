package ch3

import org.specs2.mutable._

class ListSpec extends Specification {
  "ex3.2 List#tail()" should {
    "先頭の要素を除いたListを返す" in {
      List.tail(List(1,2,3)) must_== List(2,3)
    }
  }

  "ex3.3 List#setHead()" should {
    "先頭の要素を入れ替えたListを返す" in {
      List.setHead(List(1,2,3), 5) must_== List(5,2,3)
    }
  }

  "ex3.4 List#drop()" should {
    "先頭からn個の要素を削除したListを返す" in {
      List.drop(List(1,2,3,4,5), 3) must_== List(4,5)
    }
  }

  "ex3.5 List#dropWhile()" should {
    "述語とマッチする限り先頭から要素を削除し、残ったListを返す" in {
      List.dropWhile(List(1,2,3,4,5), (n: Int) => n <= 3) must_== List(4, 5)
    }
  }

  "ex3.6 List#init()" should {
    "最後の要素を削除したListを返す" in {
      List.init(List(1,2,3)) must_== List(1,2)
    }
  }

  "ex3.9 List#length()" should {
    "Listの長さを返す" in {
      List.length(List(1,2,3)) must_== 3
    }
  }

  "ex3.10 List#foldLeft()" should {
    "List[Int]の足し算" in {
      List.foldLeft(List(1,2,3), 0)(_ + _) must_== 6
    }
  }

  "ex3.11" should {
    "List#sumFl" in {
      List.sumFl(List(1,2,3)) must_== 6
    }

    "List#productFl" in {
      List.productFl(List(2.0,3.0,5.0)) must_== 30.0
    }

    "List#lengthFl" in {
      List.lengthFl(List(1,2,3)) must_== 3
    }
  }

  "ex3.12 List#reverse()" should {
    "List[Int]に対して、要素が逆にならんだListを返す" in {
      List.reverse(List(1,2,3)) must_== List(3,2,1)
    }
  }

  "ex3.14 List#append()" should {
    "2つのListを結合する" in {
      List.append(List(1,2), List(3,4)) must_== List(1,2,3,4)
    }
  }

  "ex3.15 List#concat()" should {
    "2次元リストを1次元リストにする" in {
      List.concat(List(List(1,2), List(3,4,5), List(6,7))) must_== List(1,2,3,4,5,6,7)
    }
  }

  "ex3.16 List#addOne()" should {
    "各要素に1を足したListを返す" in {
      List.addOne(List(1,2,3)) must_== List(2,3,4)
    }
  }

  "ex3.17 List#dToS()" should {
    "Doubleの各要素をStringに変換したListを返す" in {
      List.dToS(List(1.0, 2.0, 3.0)) must_== List("1.0", "2.0", "3.0")
    }
  }

  "ex3.18 List#map()" should {
    "IntのListをStringのListに変換する" in {
      List.map(List(1,2,3))(i => i.toString()) must_== List("1", "2", "3")
    }
  }

  "ex3.19 List#filter()" should {
    "List[Int]から奇数を全て削除する" in {
      List.filter(List(1,2,3,4,5))(i => i % 2 == 0) must_== List(2,4)
    }
  }

  "ex3.20 List#flatMap()" should {
    "as: List(1,2,3), f: i => List(i,i)に適用すると、List(1,1,2,2,3,3)が返る" in {
      List.flatMap(List(1,2,3))(i => List(i, i)) must_== List(1,1,2,2,3,3)
    }
  }

  "ex3.21 List#filterFM()" should {
    "List[Int]から奇数を全て削除する" in {
      List.filterFM(List(1,2,3,4,5))(i => i % 2 == 0) must_== List(2,4)
    }
  }

  "ex3.22 List#zip()" should {
    "2つのListの要素数が同じ" in {
      List.zip(List(1,2,3), List(1,2,3)) must_== List(2,4,6)
    }

    "2つのListの要素数が異なる" in {
      List.zip(List(1,2,3), List(1,2)) must_== List(2,4)
    }
  }

  "ex3.23 List#zipWith()" should {
    "2つのList[Int]の要素を加算" in {
      List.zipWith(List(1, 2, 3), List(1, 2, 3))(_ + _) must_== List(2, 4, 6)
    }

    "2つのList[String]の要素の長さ" in {
      List.zipWith(List("aa","bb"), List("cc","dd"))((a,b) => a.length() + b.length()) must_== List(4,4)
    }
  }

}
