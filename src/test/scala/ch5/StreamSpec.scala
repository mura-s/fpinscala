package ch5

import org.specs2.mutable._

class StreamSpec extends Specification {
  "ex5.1 Stream#toList()" should {
    "StreamをListに変換" in {
      Stream(1,2,3).toList must_== List(1,2,3)
    }
  }

  "ex5.2 Stream#take()" should {
    "要素が5個のStreamから3つ取り出す" in {
      Stream(1,2,3,4,5).take(3).toList must_== List(1,2,3)
    }

    "要素が3個のStreamから5つ取り出す" in {
      Stream(1,2,3).take(5).toList must_== List(1,2,3)
    }
  }

  "ex5.2 Stream#drop()" should {
    "要素が5個のStreamの先頭からから2個取り除く" in {
      Stream(1,2,3,4,5).drop(2).toList must_== List(3,4,5)
    }
  }

  "ex5.3 Stream#takeWhile()" should {
    "先頭から条件を満たす間、要素を取り出す" in {
      Stream(1,3,2,5,4).takeWhile(_ < 5).toList must_== List(1,3,2)
    }
  }

  "ex5.4 Stream#forAll()" should {
    "全ての要素が条件を満たす" in {
      Stream(1,2,3).forAll(_ <= 3) must_== true
    }

    "条件を満たさない要素がある" in {
      Stream(1,2,3).forAll(_ < 2) must_== false
    }
  }

  "ex5.5 Stream#takeWhileFR()" should {
    "先頭から条件を満たす間、要素を取り出す" in {
      Stream(1,3,2,5,4).takeWhileFR(_ < 5).toList must_== List(1,3,2)
    }
  }

  "ex5.6 Stream#headOption()" should {
    "Someを返す" in {
      Stream(1,2,3).headOption must_== Some(1)
    }

    "Noneを返す" in {
      Stream.empty.headOption must_== None
    }
  }

  "ex5.7 Stream#map()" should {
    "すべての要素を2倍する" in {
      Stream(1,2,3).map(_ * 2).toList must_== List(2,4,6)
    }
  }

  "ex5.7 Stream#filter()" should {
    "Stream(1,4,5,2,3)から3以下の要素を返す" in {
      Stream(1,4,5,2,3).filter(_ <= 3).toList must_== List(1,2,3)
    }
  }

  "ex5.7 Stream#append()" should {
    "Streamを結合する" in {
      Stream(1,2).append(Stream(3,4)).toList must_== List(1,2,3,4)
    }
  }

  "ex5.7 Stream#flatMap()" should {
    "f: Int => Stream(Int, Int)に対して" in {
      Stream(1,2,3).flatMap(a => Stream(a, a)).toList must_== List(1,1,2,2,3,3)
    }
  }

   "ex5.8 Stream#constant()" should {
     "無限リスト作成後、先頭から5つの要素を取得する" in {
       Stream.constant(1).take(5).toList must_== List(1,1,1,1,1)
     }
   }

  "ex5.9 Stream#from()" should {
    "1ずつ増加する無限リスト作成後、先頭から5つの要素を取得する" in {
      Stream.from(5).take(5).toList must_== List(5,6,7,8,9)
    }
  }

  "ex5.10 Stream#fibs()" should {
    "フィボナッチ数列を先頭から5つ表示" in {
      Stream.fibs.take(5).toList must_== List(0,1,1,2,3)
    }
  }

  "ex5.11 Stream#unfold()" should {
    "1~5までのStreamを生成" in {
      Stream.unfold(0)(s => if (s < 5) Some(s+1, s+1) else None).toList must_== List(1,2,3,4,5)
    }
  }

  "ex5.12 Stream#fibsUF()" should {
    "フィボナッチ数列を先頭から5つ表示" in {
      Stream.fibsUF.take(5).toList must_== List(0,1,1,2,3)
    }
  }

  "ex5.12 Stream#fromUF()" should {
    "1ずつ増加する無限リスト作成後、先頭から5つの要素を取得する" in {
      Stream.fromUF(5).take(5).toList must_== List(5,6,7,8,9)
    }
  }

  "ex5.12 Stream#constantUF()" should {
    "無限リスト作成後、先頭から5つの要素を取得する" in {
      Stream.constantUF(1).take(5).toList must_== List(1,1,1,1,1)
    }
  }

  "ex5.12 Stream#onesUF" should {
    "無限リスト作成後、先頭から5つの要素を取得する" in {
      Stream.onesUF.take(5).toList must_== List(1,1,1,1,1)
    }
  }

  "ex5.13 Stream#mapUF()" should {
    "すべての要素を2倍する" in {
      Stream(1,2,3).mapUF(_ * 2).toList must_== List(2,4,6)
    }
  }

  "ex5.13 Stream#takeUF()" should {
    "要素が5個のStreamから3つ取り出す" in {
      Stream(1,2,3,4,5).takeUF(3).toList must_== List(1,2,3)
    }

    "要素が3個のStreamから5つ取り出す" in {
      Stream(1,2,3).takeUF(5).toList must_== List(1,2,3)
    }
  }

  "ex5.13 Stream#takeWhileUF()" should {
    "先頭から条件を満たす間、要素を取り出す" in {
      Stream(1,3,2,5,4).takeWhileUF(_ < 5).toList must_== List(1,3,2)
    }
  }

  "ex5.13 Stream#zipWith()" should {
    "2つのStreamの各要素同士を加算したStreamを返す" in {
      Stream(1,2,3).zipWith(Stream(1,2,3,4))(_ + _).toList must_== List(2,4,6)
    }
  }

  "ex5.14 Stream#startWith()" should {
    "Stream(1,2,3)はStream(1,2)から始まる" in {
      Stream(1,2,3).startWith(Stream(1,2)) must beTrue
    }
  }

  "ex5.15 Stream#tails()" should {
    "Stream(1,2)について" in {
      Stream(1,2).tails.toList.map(_.toList) must_== List(List(1,2), List(2), List())
    }
  }
}
