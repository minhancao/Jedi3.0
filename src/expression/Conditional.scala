package expression

import value._
import context._

case class Conditional(condition: Expression, consequent: Expression, alternative: Expression = null) extends SpecialForm {
   override def execute(env: Environment): Value = {
     val test = condition.execute(env)
     if(!test.isInstanceOf[Boole]) throw new TypeException("Condition must be type Boole")     
     if(test.asInstanceOf[Boole].value) consequent.execute(env)
     else if(null != alternative) alternative.execute(env)
     else Notification.UNDEFINED
     
   }
}