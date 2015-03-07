package scalacheck

import org.scalacheck.{Gen, Arbitrary, Properties}
import org.scalacheck.Prop._


class SortedList(list:List[Int], val ascending:Boolean) {
   val data = list.sorted(if(ascending) Ordering.Int else Ordering.Int.reverse)
}


object SortedListTests extends Properties("SortedList") {

   val genSorted = for {
      data  <- Arbitrary.arbitrary[List[Int]]
      ordering <- Gen.oneOf(true, false)
   } yield new SortedList(data, ordering)

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
