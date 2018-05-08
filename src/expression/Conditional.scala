package expression
import java.beans.Expression

import context._
import value._

case class Conditional(cond: Expression, cons: Expression, alt: Expression = null  ) extends SpecialForm {

  def execute(env: Environment): Value = {
    if(cond.execute(env).isInstanceOf[Boole])
    {
   //   cond.execute(env).asInstanceOf[Boole]
      if(cond.execute(env) == Boole(true))
      {
        cons.execute(env)
      }
      else if(alt != null)
      {
        alt.execute(env)
      }
      else Notification.UNSPECIFIED

    }
    else throw new context.TypeException();



  }

}
