package expression
import context._
import value._

case class Lambda(val params: List[Identifier], val body: Expression) extends SpecialForm {
   def execute(env: Environment): Value = {
    new Closure(params, body, env)
  }
}
