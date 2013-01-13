package widelamb


sealed abstract class Term
sealed abstract class Literal extends Term

case class Variable(name: String) extends Term
case class Application(function: Term, argument: Term) extends Term
case class Abstraction(variable: Variable, body: Term) extends Term
case class Let(variable: Variable, value: Term, body: Term) extends Term
case class Fix(variable: Variable, body: Term) extends Term
case class IntegerLiteral(value: Int) extends Literal
