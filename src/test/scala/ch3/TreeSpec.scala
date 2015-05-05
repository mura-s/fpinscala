package ch3

import org.specs2.mutable._

class TreeSpec extends Specification {
  "ex3.25 Tree#size()" should {
    "ノードの数を返す" in {
      Tree.size(Branch(Branch(Leaf("a"), Leaf("b")),
        Branch(Leaf("c"), Leaf("d")))) must_== 7
    }
  }

  "ex3.26 Tree#maximum()" should {
    "最大値を返す" in {
      Tree.maximum(Branch(Branch(Leaf(1), Leaf(3)),
        Branch(Leaf(4), Leaf(2)))) must_== 4
    }
  }

  "ex3.27 Tree#depth()" should {
    "最長パスを返す" in {
      Tree.depth(Branch(Branch(Branch(Leaf(1), Leaf(1)), Leaf(3)),
        Branch(Leaf(4), Leaf(2)))) must_== 3
    }
  }

  "ex3.28 Tree#map()" should {
    "各要素の値を2倍する" in {
      Tree.map(Branch(Branch(Leaf(1), Leaf(3)), Branch(Leaf(4), Leaf(2))))(_ * 2).
        must_==(Branch(Branch(Leaf(2), Leaf(6)), Branch(Leaf(8), Leaf(4))))
    }
  }

  "ex3.29" should {
    "Tree#sizeFold()" in {
      Tree.sizeFold(Branch(Branch(Leaf("a"), Leaf("b")),
        Branch(Leaf("c"), Leaf("d")))) must_== 7
    }

    "Tree#maximumFold()" in {
      Tree.maximumFold(Branch(Branch(Leaf(1), Leaf(3)),
        Branch(Leaf(4), Leaf(2)))) must_== 4
    }

    "Tree#depthFold()" in {
      Tree.depthFold(Branch(Branch(Branch(Leaf(1), Leaf(1)), Leaf(3)),
        Branch(Leaf(4), Leaf(2)))) must_== 3
    }

    "Tree#mapFold()" should {
      "各要素の値を2倍する" in {
        Tree.mapFold(Branch(Branch(Leaf(1), Leaf(3)), Branch(Leaf(4), Leaf(2))))(_ * 2).
          must_==(Branch(Branch(Leaf(2), Leaf(6)), Branch(Leaf(8), Leaf(4))))
      }
    }
  }
}
