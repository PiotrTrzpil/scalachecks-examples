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

   val smallVectors = Gen.choose(Vect(0, 0), Vect(5, 5))

//   property("vect length") = forAll(smallVectors) { (v: Vect) =>
//      v.length < 5
//   }


   implicit val arbVector = Arbitrary(smallVectors)

//   property("vect length") = forAll { (v: Vect) =>
//      v.length >= 0
//   }


//   property("collect example") = forAll { (v: Vect) =>
//     classify(v.length < 5, "small", "large") {
//        collect(v, v.length) {
//           v.length >= 0
//        }
//     }
//   }


}