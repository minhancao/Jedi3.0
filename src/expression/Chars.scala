package expression

import context._
import value._

case class Chars(val value: String) extends Literal with Ordered[Chars] with Equals {
  def substring(start: Integer, end: Integer) = Chars(value.substring(start.value, end.value))
  def length: Integer = Integer(value.length)
  
  def +(other: Chars): Chars = Chars(this.value + other.value)
  override def toString = value
  def compare(other: Chars): Int = this.value.compare(other.value)
  override def equals(other: Any): Boolean = 
    other match {
       case other: Chars => (other.isInstanceOf[Chars]) && (other.value == this.value)
       case _ => false
    }
  override def hashCode = this.toString.##
}