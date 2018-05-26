package context

import expression._
import value._

class Context {
  trait Context
  
  object Console {
    def repl() {}
    def execute() {}
    def main() {}
  }
}