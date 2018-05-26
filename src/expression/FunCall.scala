package expression

import context._
import value._

case class FunCall(operator: Identifier, operands: List[Expression]) extends Expression {
  def execute(env: Environment): Value = {
    val eagerExec = operands.map(_.execute(env)) //eager execution
    try {
      val a = operator.execute(env)
      if(!a.isInstanceOf[Closure]) throw new TypeException
      a.asInstanceOf[Closure].apply(eagerExec)
    }
    
    catch {
      case e: UndefinedException => alu.execute(operator, eagerExec)
    }
  }
}

