package scalacheck

import org.scalacheck.{Arbitrary, Gen, Properties}
import org.scalacheck.Prop._
import org.scalacheck._

object ListTests extends Properties("List") {

   property("prepend") = forAll { (list: List[Int]) =>
      val newList = 0 +: list
      newList.size == list.size + 1 && newList.head == 0
   }

   property("append and drop") = forAll { (list: List[Int]) =>
      (list :+ 0).dropRight(1) == list
   }

   property("concat") = forAll { (a: List[Int], b: List[Int]) =>
      (a ++ b).size == a.size + b.size
   }

   property("head") = forAll { (list: List[Int]) =>
      val propHead = (list.size > 0) ==> (list.headOption != None)
      val propFalse = Prop.falsified
      propHead || propFalse
   }

   property("false") = forAll { (list: List[Int]) =>
      ("list size should be zero" |: list.size == 0) &&
        ("size of list should be larger than zero" |: list.size > 0)
   }
}


object SortedListTests extends Properties("SortedList") {

   val genSorted = for {
      list  <- Arbitrary.arbitrary[List[Int]]
      ordering <- Gen.oneOf(true, false)
   } yield new SortedList(list, ordering)

//   implicit val arbSorted = Arbitrary {
//
//   }

   property("sorted") = forAll(genSorted) { (list: SortedList) =>
      (list.data.size > 1) ==> {
         if(list.ascending) {
            list.data
              .sliding(2)
              .forall(a=>a(0)<=a(1))
         } else {
            list.data
              .sliding(2)
              .forall(a=>a(0)>=a(1))
         }
     }
   }

}

class SortedList(list:List[Int], val ascending:Boolean) {
   val data = list.sorted(if(ascending) Ordering.Int else Ordering.Int.reverse)
}

