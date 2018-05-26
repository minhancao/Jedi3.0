package expression

import value._
import context._

//&&
case class Conjunction(conds: List[Expression]) extends Expression {
  def execute(env: Environment) = {
    var done = false
    var boolToReturn = Boole(true)
    for(cond <- conds if !done) 
    {
      val test = cond.execute(env)
      if (!test.isInstanceOf[Boole]) throw new TypeException("Conjunction conditions must be booles")
      val test2 = test.asInstanceOf[Boole]
      if (!test2.value) {
        boolToReturn = Boole(false)
        done = true
      }
    }
    boolToReturn
  }
  
}