package expression
import context._
import value._

case class Conjunction(operands: List[Expression]) extends SpecialForm {

  def execute(env: Environment): Value = {

    val ops: List[Expression] = operands.filter((op: Expression) => op.isInstanceOf[Boole]).map((op: Expression) => op.asInstanceOf[Boole])
    var result = true
    var index =  0

    if(ops.size == operands.size)
    {
      while(result && index < ops.length) {
        if (ops(index).execute(env) == Boole(true))
          index = index + 1

        else result = false
      }
    }
    else throw new TypeException()
    Boole(result)

  }
}
