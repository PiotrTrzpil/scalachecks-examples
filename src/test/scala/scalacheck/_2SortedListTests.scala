package scalacheck

import org.scalacheck.{Gen, Arbitrary, Properties}
import org.scalacheck.Prop._


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
