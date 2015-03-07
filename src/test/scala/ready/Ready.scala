package ready

import org.scalacheck.{Gen, Arbitrary, Properties}
import org.scalacheck.Prop._
import scalacheck.SortedList
import org.scalacheck.Gen.Choose
import scalacheck.VectTests.Vect


object Ready extends Properties("SortedList") {

   val ints = Arbitrary.arbitrary[Int]

   val listsLargerThanOne = Gen.nonEmptyListOf(ints)
     .map(_ :+ ints.sample.get)

   val listsLargerThanOne_2 = Gen.zip(Gen.nonEmptyListOf(ints), ints)
     .map { case (list, elem) => list :+ elem}

   val genSorted = for {
      list  <- listsLargerThanOne_2//Arbitrary.arbitrary[List[Int]]
      ordering <- Gen.oneOf(true, false)
   } yield new SortedList(list, ordering)





   implicit val choose = new Choose[Vect] {
      def choose(low: Vect, high: Vect) =
         for {
            x <- Gen.choose(low.x, high.x)
            y <- Gen.choose(low.y, high.y)
         } yield Vect(x, y)
   }

   val smallVectors = Gen.choose(Vect(0, 0), Vect(5, 5))


   implicit val arbVector = Arbitrary(smallVectors)



   property("collect example") = forAll { (v: Vect) =>
      classify(v.length < 5, "small", "large") {
         collect(v, v.length) {
            v.length >= 0
         }
      }
   }
}
