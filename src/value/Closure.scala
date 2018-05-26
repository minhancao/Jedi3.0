package value

import expression._
import context._

case class Closure(parameters: List[Identifier], body: Expression, defEnv: Environment) extends Value {
  def apply(args: List[Value]): Value = {
    if(parameters.length != args.length) throw new TypeException("lists length not equal")
    
    var localEnvi =  new Environment(defEnv) //create temp env that extends the defenv
    localEnvi.put(parameters, args) //binds the parameters to the arguments in the temp env
    body.execute(localEnvi)//execute local to the temp env
  }
}