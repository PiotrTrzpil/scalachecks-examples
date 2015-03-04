package scalacheck

import org.scalacheck.{Arbitrary, Gen, Properties}
import org.scalacheck.Gen.Choose
import org.scalacheck.Prop._


object VectTests extends Properties("Vect") {

   case class Vect(x: Double, y: Double) {
      def length = math.sqrt(x*x+y*y)
   }

   implicit val choose = new Choose[Vect] {
      def choose(low: Vect, high: Vect) =
         for {
            x <- Gen.choose(low.x, high.x)
            y <- Gen.choose(low.y, high.y)
         } yield Vect(x, y)
   }

   val smallVectors = Gen.choose(Vect(0, 0), Vect(3, 4))

   property("vect length") = forAll(smallVectors) { (v: Vect) =>
      v.length < 5
   }

   val allVectors = for {
      x <- Gen.choose(0d, Double.MaxValue)
      y <- Gen.choose(0d, Double.MaxValue)
   } yield Vect(x, y)

   implicit val arbVector = Arbitrary(allVectors)

   property("vect length") = forAll(smallVectors) { (v: Vect) =>
      v.length >= 0
   }

   property("collect example") = forAll { (v: Vect) =>
     collect(v) {
        v.length >= 0
     }
   }


}