package expression

import context._

case class Block(exps: List[Expression]) extends SpecialForm {
  def execute(env: Environment) = {
    var local = new Environment(env) //create local defining environment   
    for(i <- exps) {
      i.execute(local) 
    }
    exps(exps.length - 1).execute(local) //return the last code execution result
  }
}