package quickcheck

import common._

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  lazy val genHeap: Gen[H] = oneOf(const(empty), for {
    y <- arbitrary[Int]
  } yield insert(y, empty)
  )

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  property("insert two and get the min") = forAll { (a: A, b: A) =>
    val l = insert(a, insert(b, empty))
    findMin(l) == (if (a < b) a else b)
  }

  property("insert an element to empty and delete it will get empty") =
    forAll { (a: A) => deleteMin(insert(a, empty)) == empty }

  property("find and delete minimum continually will get sorted elements") = {
    def loop(as: H, result: H): H = {
      if (isEmpty(as)) result
      else {
        val min = findMin(as)
        loop(deleteMin(as), insert(min, result))
      }
    }

    forAll { (a: H) =>
      loop(a, empty) == a
    }
  }

  property("the minimum of melding of any two non-empty heap should be the minimum of one heap or another") =
    forAll { (a: H, b: H) => {
      (isEmpty(a), isEmpty(b)) match {
        case (false, false) => {
          val min_a = findMin(a)
          val min_b = findMin(b)
          val ab = if (min_a < min_b) meld(a, meld(b, a)) else meld(a, meld(a, b))

          val min = findMin(ab)
          min == (if (min_a < min_b) min_a else min_b)
        }
        case (_, _) => true
      }
    }
    }

  property("link of Bogus3BinomialHeap loses t1, if t1 is smaller, then findMin will fail") =
    forAll { (a: H, b: H, c: H) => {

      (isEmpty(a), isEmpty(b), isEmpty(c)) match {

        case (false, false, false) => {
          val min_a = findMin(a)
          val min_b = findMin(b)
          val min_c = findMin(c)

          if(min_a < min_b && min_b < min_c){
            val an = insert(min_b, insert(min_a, a))

            val n1 = deleteMin(deleteMin(an))
            println("n1 " + n1 + "\n")
            val bn = insert(min_a, insert(min_b, b)) // new b with min_a, min_b r = 0
            val n2 = deleteMin(deleteMin(bn))
            println("n2 " + n2 + "\n")

            findMin(n1) == findMin(n2)
          } else true
        }
        case (_, _, _) => true
      }
    }
    }
}

