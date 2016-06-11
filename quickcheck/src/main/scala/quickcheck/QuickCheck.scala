package quickcheck

import common._

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  lazy val genHeap: Gen[H] = for {
    v <- arbitrary[Int]
    h <- frequency((1, const(empty)), (10, genHeap))
  } yield insert(v, h)

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  property("link1") = forAll { (h: H, a1: A, a2: A) =>
    findMin(deleteMin(insert(a1, insert(a2, h)))) == findMin(deleteMin(insert(a2, insert(a1, h))))
  }
}
