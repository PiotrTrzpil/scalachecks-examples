package scalacheck

import org.scalacheck.{Arbitrary, Gen, Properties}

object Generators extends Properties("Generators") {

   // Special

   Gen.const("value")

   Gen.fail

   // Generator composition

   Gen.zip(Gen.alphaChar, Gen.alphaChar)

   Gen.option(Gen.alphaChar)

   Gen.frequency(10 -> Gen.choose(1, 10),
      20 -> Gen.choose(11, 100),
      70 -> Gen.choose(101, 1000))


   // Collections

   Gen.pick(3, List(1,5,6,3))

   Gen.mapOf(Gen.zip(Gen.alphaStr, Arbitrary.arbInt))

}
