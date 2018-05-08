package expression
import context._
import value._

case class Declaration(id: Identifier, exp: Expression) extends SpecialForm {
   def execute(env: Environment): Value = {
    env.put(id, exp.execute(env))
    Notification.OK
  }

}
