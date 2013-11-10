package quickcheck

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._
import scala.annotation.tailrec

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  property("min1") = forAll { a: Int =>
    val h = insert(a, empty)
    findMin(h) == a
  }

  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h))==m
  }

  property("heap returns empty") = forAll { a: Int =>
    val h = insert(a, empty)
    isEmpty(deleteMin(h))
  }

  //what if both numbers the same?
  property("heap deletes min") = forAll { pair: (Int,Int) =>
    val(a,b) = pair
    val h = insert(b,insert(a, empty))
    findMin(h) == List(a,b).min
  }

  property("heap always retrieve min") = forAll { h: H =>
    val l = h.toDescendingList
    l == l.sorted.reverse
  }

  property("melded heap returns min of 2 heaps") = forAll { pair: (H,H) =>
    val(h1,h2) = pair
   findMin(meld(h1,h2)) == List(findMin(h1),findMin(h2)).min
  }

  property("melded heap contains union of 2 heaps") = forAll { pair: (H,H) =>
    val(h1,h2) = pair
    meld(h1,h2).toDescendingList.sorted == (h1.toDescendingList:::h2.toDescendingList).sorted
  }

  lazy val genHeap: Gen[H] = for {
    i <- arbitrary[Int]
    heap <- oneOf(value(empty),genHeap)
  } yield insert(i,heap)

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  implicit class HeapTestUtil (val heap: H){

    def toDescendingList  = heap2DesList(Nil, heap)
    @tailrec
    private def heap2DesList(list: List[Int], heap: H): List[Int] = {
      if (isEmpty(heap)) list
      else {
        val l = findMin(heap)::list
        heap2DesList(l, deleteMin(heap))
      }
    }
  }

}
