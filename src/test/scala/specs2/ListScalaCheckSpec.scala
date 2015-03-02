package specs2

import org.junit.runner.RunWith
import org.specs2.mutable.Specification
import org.specs2.runner.JUnitRunner
import org.specs2.ScalaCheck
import org.specs2.matcher.ScalaCheckMatchers._
class ListScalaCheckSpec extends Specification with ScalaCheck {
  
   "A List" should {
      "should have size as a sum of two lists that were concatenated to form it" in {
         prop { (a: List[Int], b: List[Int]) =>
               a.size + b.size == (a ::: b).size
         }.display()
      }
   }

}
