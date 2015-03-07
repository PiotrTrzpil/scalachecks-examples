package scalacheck

import org.scalacheck.{Arbitrary, Gen, Properties}
import org.scalacheck.Gen.Choose
import org.scalacheck.Prop._


object VectTests extends Properties("Vect") {

   case class Vect(x: Double, y: Double) {
      def length = math.sqrt(x*x+y*y)
   }

   property("vect length") = forAll { (v: Vect) =>
      v.length >= 0
   }

}