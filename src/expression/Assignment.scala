package expression
import context._
import value._

case class Assignment(val vbl: Identifier, val update: Expression) extends SpecialForm {
  override def execute(env: Environment)= {
    val x1 = vbl.execute(env)
    val x2 = update.execute(env)
    if(x1.isInstanceOf[Variable]) {
      x1.asInstanceOf[Variable].content = x2
    }
    else throw new TypeException("Must be a variable")
    Notification.DONE

  }
}
