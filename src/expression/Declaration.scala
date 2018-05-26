package expression

import context._
import value._

case class Declaration(iden: Identifier, ex: Expression) extends SpecialForm {
  override def execute(env:Environment):Value = {
    env.put(iden, ex.execute(env))
    return Notification.OK
  }
}