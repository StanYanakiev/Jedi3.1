package expression
import value._

case class Identifier(val name: String) extends Expression {
  override def toString = name

  def execute(env: context.Environment) =
  {
    val e = env(this)
    e match
    {
      case t1: Thunk => t1.apply(List(e))
      case t2: Text => t2.body.execute(env);
      case _ => e
    }
  }
}