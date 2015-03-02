package scalacheck

import org.scalacheck.{Gen, Properties}

object Generators extends Properties("Generators") {

   // Numbers

  // Gen.chooseNum()

   Gen.choose(5, 10)



   // Collections

 //  Gen.pick()

   // Special

   Gen.const()

   Gen.fail

   // Generator composition



   Gen.option()

   Gen.wrap()
}
