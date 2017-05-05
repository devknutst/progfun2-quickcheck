package quickcheck

import common._

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._
import Math.min


abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {


  lazy val genHeap: Gen[H] = for {
    head <- arbitrary[A]
    tail <- oneOf((Gen.const(empty)), genHeap)
  } yield insert(head, tail)


  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  property("minimum") = forAll { (a: A, b: A) =>
    val heap = insert(b,insert(a, empty))
    findMin(heap) == min(a,b)
  }

  property("delete min in heap with only one") = forAll { (a: A) =>
    val heap = insert(a, empty)
    val none = deleteMin(heap)
    none == empty
  }

  property("sorted heap keep sorted if min is deleted") = forAll { (heap: H) =>
    def testOrder(x: H):Boolean =  {
      if (isEmpty(x)) true
      val old = findMin(x)
      val smaller = deleteMin(x)
      if (isEmpty(smaller)) true
      else {
        if (old <= findMin(smaller)) testOrder(smaller) else false
      }
    }
    testOrder(heap)
  }

  property("two melded heaps have a common minimum") = forAll { (h1: H, h2: H) =>

    val min1 = findMin(h1)
    val min2 = findMin(h2)
    val minAll = min(min1, min2)
    val heapAll = meld(h1, h2)
    minAll == findMin(heapAll)
  }

  property("Test meld, delete and insert.") = forAll { (h1: H, h2: H) =>
    val min1 = findMin(h1)
    val min2 = findMin(h2)
    val min = min1.min(min2)
    findMin(meld(deleteMin(h1),insert(min,h2)))==min
  }

  property("test meld min for equal") = forAll { (h1: H, h2: H) =>
    def minEqual(h1: H, h2: H): Boolean =
      if (isEmpty(h1) && isEmpty(h2)) true
      else {
        val min1 = findMin(h1)
        val min2 = findMin(h2)
        min1 == min2 && minEqual(deleteMin(h1), deleteMin(h2))
      }
    minEqual(meld(h1, h2), meld(deleteMin(h1), insert(findMin(h1), h2)))
  }



}
