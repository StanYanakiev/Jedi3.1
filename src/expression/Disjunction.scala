package expression


import value._
import context._
case class Disjunction( operands: List[Expression])extends SpecialForm {

  def execute(env: Environment): Value = {

    val ops = operands.filter((op: Expression) => op.isInstanceOf[Boole]).map((op: Expression) => op.asInstanceOf[Boole])
    var index = 0;
    var result = true
    if(ops.size == operands.size)
    {

      while(result && index < ops.length) {
        if (ops(index).execute(env) == Boole(false))
          result = false
        else index = index + 1
      }
    }
    else throw new TypeException()
    new Boole(result)
  }
}
