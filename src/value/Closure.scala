package value

import expression._
import context._

class Closure(val params: List[Identifier], val body: Expression, val defEnv: Environment, val passedEnv: Environment = new Environment() ) extends Value{
  def apply(args: List[Value]) =
  {
    var tempEnv = new Environment(defEnv)

    if(!Flags.useStaticScopeRule)
      tempEnv = new Environment(passedEnv)

    tempEnv.bulkPut(params, args)
    body.execute(tempEnv)

  }
}
