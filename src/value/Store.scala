package value

import collection.mutable._
import context._
import expression._

case class Store(var elems: ArrayBuffer[Value] = ArrayBuffer[Value]()) extends Value {
  // adds e to the end of store
  def add(e: Value): Value = {
    elems += e
    elems(elems.size-1)
    }
  // inserts e at position pos in this
  def put(e: Value, pos: Integer) {
    elems.insert(pos.value, e)
  }
  // removes element at position pos from this
  def rem(pos: Integer): Value = {
    elems.remove(pos.value)
  }
  // returns element at position pos in this
  def get(pos: Integer): Value = elems(pos.value)
  // returns true ie this contains e
  def contains(e: Value): Boole = Boole(elems.contains(e))
  // returns the size of this
  def size: Integer = Integer(elems.size)
  // returns "{e0 e1 e2 ...}"
  override def toString = {
    var strToReturn = "["
    for(i <- elems) {
      strToReturn += " " + i
    }
    
    strToReturn += "]"
    strToReturn
  }
  // returns store containing the elements of this transformed by trans
  def map(trans: Closure): Store = {
    for (i <- 0 to elems.size - 1)
    {
      elems(i) = trans.apply(List(elems(i)))
    } 
    this  
  }
  // returns store containing the elements of this that passed test
  def filter(test: Closure): Store = {
    var booleList = ArrayBuffer[Value]()
    elems.copyToBuffer(booleList)
     for (i <- booleList)
    {
      if(!test.apply(List(i)).asInstanceOf[Boole].value)
      {
        elems -= i    
      }
    } 
    this 
  }
}

