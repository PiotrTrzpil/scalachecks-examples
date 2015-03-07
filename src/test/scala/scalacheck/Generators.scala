package scalacheck

import org.scalacheck.{Gen, Properties}

object Generators extends Properties("Generators") {

   // Numbers

  // Gen.chooseNum()

   val fromRangeGen = Gen.choose(5, 10)



   // Collections

 //  Gen.pick()

   // Special

   val constantValueGen = Gen.const("value")

   Gen.fail

   // Generator composition



   Gen.option()

   Gen.wrap()
}
