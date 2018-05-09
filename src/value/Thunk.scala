package value

import context._
import expression._

//by name
class Thunk(body: Expression, defEnv: Environment) extends Closure(Nil, body, defEnv)
{
   override def apply(args: List[Value]) = {
    val  tempEnv = new Environment(defEnv)
    body.execute(tempEnv)
  }
}

