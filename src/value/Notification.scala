package value

class Notification(msg: String) extends Value{
  override def toString: String = msg;

}


object Notification extends Value{
  def apply(msg: String) = new Notification(msg)
  val DONE = Notification("DONE")
  val OK = Notification("OK")
  val UNSPECIFIED = Notification("UNSPECIFIED")
}