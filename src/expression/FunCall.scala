package expression

import context._
import value._

case class FunCall(val id: Identifier, val ops: List[Expression]) extends Expression {

  def execute(env: Environment): Value = {
    var env1 = env
    if(!Flags.useStaticScopeRule)
      env1 = new Environment()

    var arguments: List[Value] = List()
    val p = Flags.paramaterPassing

    p match {
      case Flags.passByValue => arguments = ops.map(_.execute(env1))
      case Flags.passByName => arguments = ops.map((x: Expression) => new Thunk(x, env1))
      case Flags.passByText => arguments = ops.map((x: Expression) => new Text(x))
    }
    if (env1.contains(id)) {
      val maybeClosure = id.execute(env1)
      if (maybeClosure.isInstanceOf[Closure]) {
        val closure = maybeClosure.asInstanceOf[Closure]
          closure.apply(arguments)
      }
      else {
        throw new TypeException("only functions can be called")
      }
    }
    else {
      val args = ops.map(_.execute(env1))
      alu.execute(id, args)
    }
  }

}




