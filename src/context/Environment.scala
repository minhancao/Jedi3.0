package context

import scala.collection.mutable._
import value._
import expression._

class Environment(var extension: Environment = null)
   extends HashMap[Identifier, Value] with Value {
  
  override def apply(name: Identifier): Value = {
    if (this.contains(name)) super.apply(name)
    else if (extension != null) extension.apply(name)
    else throw new UndefinedException(name)
  }
  
  def put(iden: List[Identifier], vals: List[Value]) {
    if (iden.length != vals.length) throw new TypeException("# arguments != #parameters")
    for(i <- 0 until iden.length) this.put(iden(i), vals(i))
  }
}