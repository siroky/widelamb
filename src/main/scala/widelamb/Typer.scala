package widelamb

import util.parsing.combinator.{PackratParsers, JavaTokenParsers}
import util.parsing.input.CharSequenceReader

trait Typer {

    type Substition = Map[TypeVariable, Type]
    type Result = Either[String, (Context, Substition, TypeScheme)]

    val initialContext = new Context(
        Map[Variable, TypeScheme](
            Variable("ifzero") -> {
                val alpha = uniqueTypeVariable()
                ForAll(alpha, Function(List(Integer, alpha, alpha), alpha))
            },
            Variable("plus") -> Function(List(Integer, Integer), Integer),
            Variable("minus") -> Function(List(Integer, Integer), Integer),
            Variable("div") -> Function(List(Integer, Integer), Integer),
            Variable("mul") -> Function(List(Integer, Integer), Integer)
        )
    )

    val identitySubstitution: Substition = Map.empty[TypeVariable, Type]

    def computeType(term: Term): Either[String, TypeScheme] =
        computeTermType(initialContext, term).right.map(_._3)

    def computeTermType(context: Context, term: Term): Result = term match {
        case v: Variable => computeVariableType(context, v)
        case _ => ???
    }

    def computeVariableType(context: Context, variable: Variable): Result = {
        context.items.get(variable) match {
            case Some(typeScheme) => {
                val boundTypeVariables = getBoundTypeVariables(typeScheme)
                ???
                //val substitution = boundTypeVariables.map(v => (v, uniqueTypeVariable())).toMap[TypeScheme, TypeScheme]
                //Right(context, identitySubstitution, applySubstitution(tau, substitution))
            }
            case None => Left(s"Can't find type of the free variable '$variable'.")
        }
    }

    def getBoundTypeVariables(tau: TypeScheme): List[TypeVariable] = tau match {
        case ForAll(variable, body) => variable :: getBoundTypeVariables(body)
        case _ => Nil
    }

    def freeTypeVariables(typeScheme: TypeScheme): Set[TypeVariable] = typeScheme match {
        case ForAll(variable, body) => freeTypeVariables(body) - variable
        case Function(domain, range) => freeTypeVariables(domain) union freeTypeVariables(range)
        case variable: TypeVariable => Set(variable)
        case _ => Set.empty
    }

    def substitute(s: Substition, typeScheme: TypeScheme): TypeScheme = {
        require(s.keys.toSet subsetOf freeTypeVariables(typeScheme))
        typeSchemeSubstitution(s, typeScheme)
    }

    def typeSchemeSubstitution(s: Substition, typeScheme: TypeScheme): TypeScheme = typeScheme match {
        case ForAll(typeVariable, body) => ForAll(typeVariable, typeSchemeSubstitution(s, body))
        case tpe: Type => typeSubstitution(s, tpe)
    }

    def typeSubstitution(s: Substition, tpe: Type): Type = tpe match {
        case Function(domain, range) => Function(typeSubstitution(s, domain), typeSubstitution(s, range))
        case typeVariable: TypeVariable => s.withDefault(v => v)(typeVariable)
        case _ => tpe
    }

    class Context(val items: Map[Variable, TypeScheme]) {
        lazy val freeTypeVariables: Set[TypeVariable] = items.values.flatMap(Typer.this.freeTypeVariables _).toSet

        def substituted(s: Substition): Context = {
            new Context(items.mapValues(t => substitute(s, t)))
        }

        def closureOf(typeScheme: TypeScheme): TypeScheme = {
            (Typer.this.freeTypeVariables(typeScheme) diff freeTypeVariables).toList match {
                case Nil => typeScheme
                case typeVariables => ForAll(typeVariables, typeScheme)
            }
        }
    }

    private var typeVariableId = 0
    def uniqueTypeVariable() = {
        typeVariableId += 1
        TypeVariable(s"T$typeVariableId")
    }
}
