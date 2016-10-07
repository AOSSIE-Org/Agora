package countvotes.structures

import java.math.{BigInteger}

class Rational(n: BigInt, d: BigInt) {
  
  require(d != 0)
  
  private def gcd(a: BigInt, b:BigInt): BigInt = {
    if (b == 0) a else gcd(b , a % b)
  }
  private val g = gcd(n.abs, d.abs)
  
  val numerator = ( if (d < 0) -n else n ) / g
  val denominator = d.abs/g
  
  
  
  def +(that: Rational) = 
    new Rational(numerator * that.denominator + that.numerator * denominator, denominator * that.denominator)
  def -(that: Rational) = 
    new Rational(numerator * that.denominator - that.numerator * denominator, denominator * that.denominator)
  def *(that: Rational) = 
   new Rational(numerator * that.numerator, denominator * that.denominator)
  def /(that: Rational) = 
    new Rational(numerator * that.denominator, denominator * that.numerator)
  def <(that: Rational): Boolean = 
    numerator * that.denominator < that.numerator * denominator
  def <=(that: Rational): Boolean = 
    numerator * that.denominator <= that.numerator * denominator
  def >(that: Rational): Boolean = 
    numerator * that.denominator > that.numerator * denominator
  def >=(that: Rational): Boolean = 
    numerator * that.denominator >= that.numerator * denominator
  def ==(that: Rational): Boolean = 
    numerator * that.denominator == that.numerator * denominator
    
  override def equals(other: Any): Boolean = 
    other match {
      case that: Rational => 
        (that canEqual this) &&
        numerator == that.numerator &&
        denominator == that.denominator
      
      case _ => false
    }
  
  def canEqual(other: Any): Boolean = 
      other.isInstanceOf[Rational]
  
  override def hashCode: Int =
    41 * (41 + numerator.toInt) + denominator.toInt
  
  override def toString = if (denominator == 1) numerator.toString else numerator + "/" + denominator // + " : " +  (numerator/denominator).toString()
  //  override def toString = if (denominator == 1) numerator.toString else (numerator/denominator).toString()
  
  def toInt = (numerator/denominator ).toInt
}

object Rational{

  implicit def intToRational(n: Int): Rational = new Rational(n, 1)
  
  implicit def longToRational(n: Long): Rational = new Rational(n, 1)
  
  
  
  
  def apply(n: Int): Rational = new Rational(n, 1)
  
  def apply(n: Int, d:Int): Rational = new Rational(n,d)
  
  def apply(n: BigInt, d:BigInt): Rational = new Rational(n,d)
  
  implicit val ord = new Ordering[Rational] {
    def compare(a: Rational, b: Rational) =  a.numerator * b.denominator compare b.numerator * a.denominator 
  }
  
  
  // The Code below is borrowed from
  // https://github.com/malyzajko/ceres/blob/master/src/Rational.scala
  
  implicit def doubleToRational(d: Double): Rational = fromReal(d)
  
  
  def fromReal(dbl: Double): Rational = {
    val (n, d) = realToRational(dbl.toString)
    Rational(n.toInt, d.toInt)
  }

  //Takes a string representing a real value and returns a fraction equal to it.
  // We assume this string comes directly from variable.toString, so it does not
  // have trailing zeroes, always has one decimal point, etc.
  // This works because of a property of the IEEE 754 standard that requires that
  // one can recover the exact string by going to double and back.
  private def realToRational(value: String): (BigInt, BigInt) = {

    // scientific notation
    if (value.contains("e") || value.contains("E")) {
      val splitExponent = value.split(Array('e', 'E'))

      val nom = new BigInt(new BigInteger(splitExponent(0).replace(".", "")))
      val splitDecimal = splitExponent(0).split('.')
      val denPower = splitDecimal(1).length - splitExponent(1).toInt
      
      if (denPower > 0) {
        val den = new BigInt(new BigInteger("10").pow(denPower))
        (nom, den)
      } else {
        val den = new BigInt(new BigInteger("1"))
        val newNom = nom * new BigInt(new BigInteger("10").pow(-denPower))
        (newNom, den)
      }
    }
    // decimal notation
    else {
      // numerator as simply all digits without the decimal sign
      val nom = new BigInt(new BigInteger(value.replace(".", "")))
      val parts = value.split('.')
      // can this overflow?
      val den = new BigInt(new BigInteger(math.pow(10, parts(1).length).toLong.toString))
      (nom, den)
    }

  }
  
}
