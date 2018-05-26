package expression

import context._
import value._
//||
case class Disjunction(conds :List[Expression]) extends Expression {
  def execute(env: Environment) = {
    var done = false
    var boolToReturn = Boole(false)
    for(cond <- conds if !done) 
    {
      val test = cond.execute(env)
      if (!test.isInstanceOf[Boole]) throw new TypeException("Conjunction conditions must be booles")
      val test2 = test.asInstanceOf[Boole]
      if (!test2.value) {
        boolToReturn = Boole(true)
        done = true
      }
    }
    boolToReturn
  } 
}