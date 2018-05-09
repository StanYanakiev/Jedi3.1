package expression
import context.Environment
import value._
case class Iteration(val cond: Expression, val body: Expression) extends SpecialForm {

  //execute(): execute condition. if true, execute body and
  // come back and execute the condition again.
  // use scala’s while loop. mess around with types

  override def execute(env: Environment): Value = {
    while(cond.execute(env) == Boole(true))
      body.execute(env)
    Notification.DONE
  }
}
