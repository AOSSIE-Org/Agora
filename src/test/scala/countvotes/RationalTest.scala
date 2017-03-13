package countvotes

import org.specs2.mutable.Specification
import countvotes.structures.Rational 

class RationalTest extends Specification {
   val a = Rational(1,3)
   val b = Rational(1,2)

  "Rational number" should {
    "add" in {
      a+b shouldEqual Rational(5,6)
    }

    "subtract" in {
      a-b shouldEqual Rational(-1,6)
    }

    "multiply" in {
      a*b shouldEqual Rational(1,6)
    }

    "divide" in {
      a/b shouldEqual Rational(2,3)
    }

    "convert from real" in {
      Rational.fromReal(2.25) shouldEqual Rational(9,4)
    }
  }
}
