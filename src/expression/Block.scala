package expression
import context.Environment
import value.Value

case class Block(val exp: List[Expression]) extends SpecialForm {

   def execute(env: Environment): Value =
  {
    val tempEnv = new Environment(env)
    val values = exp.map(_.execute(tempEnv))
    values(exp.size -1)
  }

}
