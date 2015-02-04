package quickcheck

import common._

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  property("min1") = forAll { a: Int =>
    val h = insert(a, empty)
    findMin(h) == a
  }

  property("min2") = forAll { (a: Int, b: Int) =>
    val h1 = insert(a, insert(a, empty))
    val h2 = insert(b, h1)
    findMin(h2) == Math.min(a, b)
  }

  property("link1") = forAll { (a: Int, b: Int) =>
    val h = insert(a, insert(b, empty))
    findMin(h) == Math.min(a, b)
  }

  property("link2") = forAll { (a: Int, b: Int, c: Int) =>
    val h1 = insert(a, insert(b, empty))
    val h2 = insert(c, empty)
    val h = deleteMin(meld(h1, h2))

    val xs = List(a, b, c) sorted

    findMin(h) == xs(1)
  }

  lazy val genHeap: Gen[H] =  for {
    k <- arbitrary[Int]
    h <- oneOf(value(empty), genHeap)
  } yield insert(k, h)

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

}
