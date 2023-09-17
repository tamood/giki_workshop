package sevenSegmentDriver

import scala.language.postfixOps

object Digits{
  private val zero =
    """
      .   _
      .  | |
      .  |_|
      """

  private val one =
    """
      .
      .    |
      .    |
      """

  private val two =
    """
      .   _
      .   _|
      .  |_
      """

  private val digits = Seq(zero, one, two)

  def apply(num: Int) = digits(num)
}

//Scala best practices Add "extends Product with Serializable" to every sealed traits used to define objects/sealed types
sealed trait DecorType extends Product with Serializable
case object DecLeft extends DecorType
case object DecRight extends DecorType
case object DecMid extends DecorType


object Decor{
  private val left =
    """
      .   _
      .  |
      .  |_
      """

  private val mid =
    """
      .   _
      .
      .   _
      """

  private val right =
    """
      .   _
      .    |
      .   _|
      """

  private val decors = {Seq(DecLeft, DecMid, DecRight) zip Seq(left, mid, right)} toMap

  def apply(dtype: DecorType) = decors(dtype)
}

class SevenSegmentDigit(pattern: String){
  def this(num: Int) = {
    this(Digits(num))
  }

  private val pSeq = {
    val table = pattern.split('.').tail.map {
      case row => {
        row.toCharArray
      }
    }
    //Indices of a-f in 3x3 character grid
    val indices = Seq((0, 1), (1, 2), (2, 2), (2, 1), (2, 0), (1, 0), (1, 1))
    indices map { case (r, c) => "|_".contains(table(r)(c + 2)) }
  }

  private val pSeq_inv = pSeq map {i=> !i}

  def getCommonCathodePattern() = pSeq
  def getCommonAnodePattern() = pSeq_inv
}

object DecorRightObj extends SevenSegmentDigit(Decor(DecRight))
