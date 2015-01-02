package quickcheck

import common._

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._
import Math._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  lazy val genHeap: Gen[H] = {
    for {
//      hFre: Boolean <- Gen.frequency((9, true), (1, false))
//      nVal: Int <- Gen.choose(0, 200)
//      h: H <- if (hFre) genHeap else empty
      n <- arbitrary[Int]
      h <- Gen.frequency((1, value(empty)), (9, genHeap))
//      h <- Gen.lzy(Gen.frequency((1, value(empty)), (9, genHeap)))
    } yield insert(n, h)
  }

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  property("hint1") = forAll { (n1: A, n2: A) =>
    val h = insert(n1, insert(n2, empty))
    findMin(h) == min(n1, n2)
  }

  property("hint2") = forAll { (n: A) =>
    isEmpty(deleteMin(insert(n, empty)))
  }

  property("hint3") = forAll { (h: H) =>
    def isSorted(h: H): Boolean =
      if (isEmpty(h)) true
      else {
        val m = findMin(h)
        val h2 = deleteMin(h)
        isEmpty(h2) || (m <= findMin(h2) && isSorted(h2))
      }
    isSorted(h)
  }

  property("hint4") = forAll { (h1: H, h2: H) =>
    findMin(meld(h1, h2)) == min(findMin(h1), findMin(h2))
  }

  /* https://class.coursera.org/reactive-001/forum/thread?thread_id=97#post-371 */
  property("meld") = forAll { (h1: H, h2: H) =>
    def heapEqual(h1: H, h2: H): Boolean =
      if (isEmpty(h1) && isEmpty(h2)) true
      else {
        val m1 = findMin(h1)
        val m2 = findMin(h2)
        m1 == m2 && heapEqual(deleteMin(h1), deleteMin(h2))
      }
    heapEqual(meld(h1, h2),
      meld(deleteMin(h1), insert(findMin(h1), h2)))
  }

  property("min1") = forAll { a: Int =>
    val h = insert(a, empty)
    findMin(h) == a
  }

  property("min2") = forAll { (a: A, b: A) =>
    val h1 = insert(a, empty)
    val h2 = insert(b, empty)
    ( a < b ) ==> ( findMin(h1) < findMin(h2) ) :| ""
  }

  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h))==m
  }

  property("delete1") = forAll { (a: A) =>
    val h = insert(a, empty)
    val hn = deleteMin(h)
    (isEmpty(hn)) :| "empty heal fail"
  }

  property("sort2") = forAll { (h: H) =>
    def ensureMin(min: A, h: H): Boolean = {
      if (isEmpty(h)) {
        true
      }
      else {
        val minN = findMin(h)
        val hN = deleteMin(h)
        ( min <= minN ) && ensureMin(minN, hN)
      }
    }
    (!isEmpty(h)) ==> ensureMin(findMin(h), deleteMin(h))
  }

  property("sort1") = forAll { (h: H) =>
    if (! isEmpty(h)) {
      val min: A = findMin(h)
      val h2: H = deleteMin(h)
      if (! isEmpty(h2)) {
        val minN = findMin(h2)
        min <= minN
      }
    }
    true
  }

  property("meld1") = forAll { (h1: H, h2: H) =>
    val h3 = meld(h1, h2)
    if (isEmpty(h1) && isEmpty(h2)) {
      h3 == empty
    }
    else if (isEmpty(h1) && !isEmpty(h2)) {
      val min2 = findMin(h2)
      val min3 = findMin(h3)
      min2 == min3
    }
    else if (!isEmpty(h1) && isEmpty(h2)) {
      val min1 = findMin(h1)
      val min3 = findMin(h3)
      min1 == min3
    }
    else {
      val min1 = findMin(h1)
      val min2 = findMin(h2)
      val min3 = findMin(h3)
      if (min1 < min2) min3 == min1
      else min3 == min2
    }
  }

//  lazy val genMap: Gen[Map[Int,Int]] = for {
//    k <- arbitrary[Int]
//    v <- arbitrary[Int]
//    m <- oneOf(value(Map.empty[Int,Int]), genMap)
//  } yield m.updated(k, v)
}
