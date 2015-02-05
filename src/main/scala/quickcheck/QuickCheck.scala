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

  property("deleteMin") = forAll { a: Int =>
    val h = insert(a, empty)
    val e = deleteMin(h)
    e == empty
  }

  property("sortedMinimums") = forAll { h: H =>
    val sortedMinimums = deleteAndGetAll(h, Nil).reverse
    sortedMinimums == sortedMinimums.sorted
  }

  def deleteAndGetAll (h: H, acc: List[A]) :List[A] = {
    if (h != empty) {
      val newAcc = findMin(h) :: acc
      deleteAndGetAll(deleteMin(h), newAcc)
    }
    else
      acc
  }

  property("findMinFromMeld") = forAll { (h1: H, h2: H) =>
    findMin(meld(h1, h2)) == Math.min(findMin(h1), findMin(h2))
  }

  lazy val genHeap: Gen[H] =  for {
    k <- arbitrary[Int]
    h <- oneOf(value(empty), genHeap)
  } yield insert(k, h)

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

}
