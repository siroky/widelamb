package widelamb

sealed abstract class TypeScheme
sealed abstract class Type extends TypeScheme
sealed abstract class TypeConstant extends Type

case class ForAll(variable: TypeVariable, body: TypeScheme) extends TypeScheme {
    override def toString = s"(V$variable . $body)"
}

object ForAll {
    def apply(variables: List[TypeVariable], body: TypeScheme): ForAll = {
        require(variables.nonEmpty)
        variables match {
            case variable :: Nil => ForAll(variable, body)
            case variable :: rest => ForAll(variable, ForAll(rest, body))
        }
    }
}

case class TypeVariable(name: String) extends Type {
    override def toString = name
}

case class Function(domain: Type, range: Type) extends Type {
    override def toString = s"($domain -> $range)"
}

object Function {
    def apply(domains: List[Type], range: Type): Function = {
        require(domains.nonEmpty)
        domains match {
            case domain :: Nil => Function(domain, range)
            case domain :: rest => Function(domain, Function(rest, range))
        }
    }
}

case object Integer extends TypeConstant {
    override def toString = "Int"
}
