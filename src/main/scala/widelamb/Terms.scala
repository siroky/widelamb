package widelamb

sealed abstract class Term
sealed abstract class Literal(val tpe: TypeConstant) extends Term

case class Variable(name: String) extends Term {
    override def toString = name
}

case class Application(function: Term, argument: Term) extends Term {
    override def toString = s"($function $argument)"
}

case class Abstraction(variable: Variable, body: Term) extends Term {
    override def toString = s"(\\$variable . $body)"
}

case class Let(variable: Variable, value: Term, body: Term) extends Term {
    override def toString = s"(let $variable = $value in $body)"
}

case class Fix(variable: Variable, body: Term) extends Term {
    override def toString = s"(fix $variable . $body)"
}

case class IntegerLiteral(value: Int) extends Literal(Integer) {
    override def toString = value.toString
}
