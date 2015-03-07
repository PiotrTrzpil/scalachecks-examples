package scalacheck

import org.scalacheck.{Arbitrary, Gen, Properties}
import org.scalacheck.Prop._
import org.scalacheck._
import org.scalacheck.Gen.Choose

object ListTests extends Properties("List") {

   property("prepending an element should increase the size by one") = forAll { (list: List[Int]) =>
      val newList = 0 +: list
      newList.size == list.size + 1 && newList.head == 0
   }

   property("appending and dropping an element at the end should yield the initial list") =
     forAll { (list: List[Int]) =>
      (list :+ 0).dropRight(1) == list
   }

   property("concatenating two lists should create a list with size equal to sum of their sizes") =
      forAll { (a: List[Int], b: List[Int]) =>
         (a ++ b).size == a.size + b.size
      }

   property("head") = forAll { (list: List[Int]) =>
      val propHead = (list.size > 0) ==> (list.headOption != None)
      val propFalse = Prop.falsified
      propHead || propFalse
   }

//   property("false") = forAll { (list: List[Int]) =>
//      ("list size should be zero" |: list.size == 0) &&
//        ("size of list should be larger than zero" |: list.size > 0)
//   }
}




