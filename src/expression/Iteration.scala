package expression

import context._
import value._

case class Iteration(condition: Expression, body: Expression, top: Boolean = true) extends SpecialForm {
  def execute(env: Environment) = {
    var end: Value = if (top) Notification.UNSPECIFIED else body.execute(env)
    val cond = condition.execute(env)
    if(!cond.isInstanceOf[Boole]) throw new TypeException("Condition must be a Boole!")
    var cond2 = cond.asInstanceOf[Boole]
      while(cond2.value)
      {
        end = body.execute(env)
        cond2 = condition.execute(env).asInstanceOf[Boole]
      }
      end
    
  }
}