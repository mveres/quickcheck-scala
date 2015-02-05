import common._

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._
import quickcheck._

lazy val genMap: Gen[Map[Int,Int]] = for {
  k <- arbitrary[Int]
  v <- arbitrary[Int]
  m <- oneOf(value(Map.empty[Int,Int]), genMap)
} yield m.updated(k, v)

val s = genMap.sample


val h = new BinomialHeap with IntHeap

val h1 = h.insert(1, h.empty)
val h2 = h.insert(2, h1)
val h3 = h.insert(3, h2)
val h4 = h.insert(4, h.empty)

h.findMin(h3)

val dh = h.meld(h3, h2)
val l = h.deleteMin(dh)
h.findMin(l)

val qch = new QuickCheckHeap with BinomialHeap

val randomHeap = qch.genHeap.sample.get

qch.findMin(randomHeap)




