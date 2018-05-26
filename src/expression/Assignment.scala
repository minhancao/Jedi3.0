package expression

import context._
import value._

case class Assignment(vbl: Identifier, update: Expression) extends SpecialForm {
  def execute(env: Environment) = {
    var v1 = vbl.execute(env)
    if(!v1.isInstanceOf[Variable]) throw new TypeException("Must be a variable for Assignment!")
    else {
      v1.asInstanceOf[Variable].content = update.execute(env)
      Notification.DONE
    }
  }
}