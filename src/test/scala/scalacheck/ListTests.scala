package scalacheck

import org.scalacheck.Properties
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

class SortedList(list:List[Int], val ascending:Boolean) {
   val data = list.sorted
}

