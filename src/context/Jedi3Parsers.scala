package context



import scala.util.parsing.combinator._
import expression.{Expression, _}
import value._

class Jedi3Parsers extends Jedi2Parsers {

  // assignment ::= identifier ~ "=" ~ expression
  def assignment: Parser[Assignment] = identifier ~  "=" ~ expression ^^{
    case vrl ~ "=" ~ upd => Assignment(vrl, upd)
  }
  // iteration ::= "while" ~ "(" ~ expression ~ ")" ~ expression
  def iteration: Parser[Iteration] = "while" ~ "(" ~ expression ~ ")" ~ expression ^^{
    case "while" ~ "(" ~ cond ~ ")" ~ body => Iteration(cond, body)
  }

  // dereference ::= "[" ~ expression ~ "]"
  def dereference: Parser[Expression] = "[" ~ expression ~ "]" ^^ {
    case "[" ~ exp ~ "]" => FunCall(Identifier("dereference"),List(exp))
  }



  override def expression: Parser[Expression] = declaration | conditional | iteration | disjunction | failure("Invalid expression")
  override def term: Parser[Expression]  = lambda | funCall | block | assignment | dereference | literal | "("~>expression<~")"
}