package scalacheck

import org.scalacheck.Prop.forAll
import org.scalacheck.Prop.propBoolean
import scala.util.Random
import org.scalacheck._
object ListSpec extends Properties("List") {

//   property("list") = forAll { (a: List[Int]) =>
//      (a.size > 4) ==>{
//        val sorted = Test.quicksort(a)
//        sorted.sliding(2).filter(_.size > 1).forall(l => l(0) < l(1))
//     }
//   }

   property("partition") = forAll { (a: List[Int]) =>
      (a.size > 4 ) ==> {
         val ar = a.toArray
         val pivot = Test.partition(ar)
         val list = ar.toList

         Prop(list.forall(a.contains(_)) ) && {
            (pivot >= 0) ==> {
               val (left, right) = list.splitAt(pivot)
               left.forall(_ <= list(pivot)) && right.forall(_ >= list(pivot))
            }
         }
      }
   }
}

object Test {
   def quicksort(input: List[Int]): List[Int] = {
      println("QUICKSORT on: "+input)
      def loop(input: Array[Int]) : Unit= {
         val pivot = partition(input)
         println("after partition: "+input.toList+" pivot: "+pivot)
         if (pivot >= 0) {
            loop(input.slice(0, pivot))
            loop(input.slice(pivot, input.length))
         }
      }

      val arr = input.toArray
      loop(arr)
      println("QUICKSORT FINISHED: "+arr.toList.toString())
      arr.toList
   }
   def partition(input: Array[Int]):Int = {
      if (input.length <= 1) {
         -1
      } else {
         val pivotIndex = Random.nextInt(input.length)
         val pivot = input(pivotIndex)
      //   println("input list: "+input.toList.toString+" pivot index: "+pivotIndex)
         swap(input, 0, pivotIndex)
         var firstToSwap = 1
         for (i <- firstToSwap until input.length) {
            if (input(i) < pivot) {
               swap(input, firstToSwap, i)
        //       println("After swap: "+input.toList.toString)
               firstToSwap = firstToSwap + 1
            }
         }
         swap(input, 0, firstToSwap - 1)
      // /  println("result partition: "+input.toList.toString)
         firstToSwap - 1
      }
   }

   def swap(input: Array[Int], a:Int, b:Int) = {
      val tmp = input(a)
      input.update(a,input(b))
      input.update(b,tmp)

   }
}