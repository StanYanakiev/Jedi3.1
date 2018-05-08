package value

case class Boole(val value: Boolean) extends expression.Literal with Value {

  override def toString = value.toString

  def &&(other: Boole) = Boole(other.value && this.value)
  def ||(other: Boole) = Boole(other.value || this.value)
  def unary_!() : Boole = if(this.value) Boole(false) else Boole(true)

}

