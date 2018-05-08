package value

class Variable(val content: Value) extends Value{

  override def toString: String = content.toString

}
