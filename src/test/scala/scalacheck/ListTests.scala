package scalacheck

import org.scalacheck.{Arbitrary, Gen, Properties}
import org.scalacheck.Prop._


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
}


object SortedListTests extends Properties("SortedList") {

   implicit val gen = Arbitrary(for {
     list  <- Arbitrary.arbitrary[List[Int]]
      ordering <- Gen.oneOf(true, false)
   } yield new SortedList(list, ordering))

   property("sorted") = forAll { (list: SortedList) =>
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

