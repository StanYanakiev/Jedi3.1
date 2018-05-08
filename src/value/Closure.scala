package value

import expression._
import context._

class Closure(val params: List[Identifier], val body: Expression, val defEnv: Environment) extends Value{
  def apply(args: List[Value], env: Environment) =
  {
    var tempEnv = new Environment(defEnv)

    if(!Flags.useStaticScopeRule)
      tempEnv = new Environment(env)

    tempEnv.bulkPut(params, args)
    body.execute(tempEnv)

  }
}
