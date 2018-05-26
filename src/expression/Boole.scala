package expression


import value._
import context._

trait Expression extends Value {
  def execute(env: Environment): Value
}

/**********************************/

trait Literal extends Expression with Value {
  def execute(env: Environment) = this
}

trait SpecialForm extends Expression 

case class Boole(val value: Boolean) extends Literal with Equals  {
  def &&(other: Boole) = Boole(this.value && other.value)
  def ||(other: Boole) = Boole(this.value || other.value)
  // *, -, /
  def unary_! = !this.value // unary negation
  override def toString = value.toString
  def compare(other: Boole): Int = if (this.value < other.value) -1 else if (other.value < this.value) 1 else 0
  override def canEqual(other: Any) =  other.isInstanceOf[Boole]
  override def equals(other: Any): Boolean = 
    other match {
       case other: Boole => this.canEqual(other) && (other.value == this.value)
       case _ => false
    }
  override def hashCode = this.toString.##
}

object Boole {
  implicit def intToBoole(n: Boole): Boole = Boole(n.value)
}


  
