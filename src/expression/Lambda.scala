package expression

import context._
import value._

case class Lambda(parameters: List[Identifier], body: Expression) extends SpecialForm {
  def execute(env: Environment) = {
    new Closure(parameters, body, env)
  }
}