package value

import expression._

trait Value

class Notification(val value: String) extends Value {
  
}

object Notification {
  var DONE = Chars("DONE")
  var UNSPECIFIED = Chars("UNSPECIFIED")
  var OK = Chars("OK")
  var UNDEFINED = Chars("UNDEFINED")
    def apply(value: String) = 
        new Notification(value)
  }