package widelamb

trait Typer {

    type Result = Either[String, (Substitution, Type)]

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

    def computeType(term: Term): Either[String, Type] = computeTermType(initialContext, term).right.map(_._2)

    def computeTermType(context: Context, term: Term): Result = term match {
        case literal: Literal => Right(IdentitySubstitution, literal.tpe)
        case variable: Variable => computeVariableType(context, variable)
        case abstraction: Abstraction => computeAbstractionType(context, abstraction)
        case let: Let => computeLetType(context, let)
        case _ => ???
    }

    def computeVariableType(context: Context, variable: Variable): Result = {
        context.get(variable) match {
            case Some(sigma) => Right(IdentitySubstitution, renameAndDropForAlls(sigma))
            case None => Left(s"Can't find type of the free variable '$variable'.")
        }
    }

    def computeApplicationType(context: Context, application: Application): Result = {
        computeTermType(context, application.function).right.flatMap { case (s1, tau1) =>
            computeTermType(context.substituted(s1), application.argument).right.flatMap { case (s2, tau2) =>
                // TODO
                Right(IdentitySubstitution, tau2)
            }
        }
    }

    def computeAbstractionType(context: Context, abstraction: Abstraction): Result = {
        val alpha = uniqueTypeVariable()
        val updatedContext = context.updated(abstraction.variable, alpha)
        computeTermType(updatedContext, abstraction.body).right.flatMap { case (s1, tau1) =>
            Right(s1, Function(s1(alpha), tau1))
        }
    }

    def computeLetType(context: Context, let: Let): Result = {
        computeTermType(context, let.value).right.flatMap { case (s1, tau1) =>
            val substitutedContext = context.substituted(s1)
            val variableScheme = substitutedContext.closureOf(tau1)
            val updatedContext = substitutedContext.updated(let.variable, variableScheme)
            computeTermType(updatedContext, let.body).right.flatMap { case (s2, tau2) =>
                Right(s2 compose s1, tau2)
            }
        }
    }

    def freeTypeVariables(sigma: TypeScheme): Set[TypeVariable] = sigma match {
        case ForAll(variable, body) => freeTypeVariables(body) - variable
        case Function(domain, range) => freeTypeVariables(domain) union freeTypeVariables(range)
        case variable: TypeVariable => Set(variable)
        case _ => Set.empty
    }

    def renameAndDropForAlls(sigma: TypeScheme, s: SimpleSubstitution = IdentitySubstitution): Type = sigma match {
        case ForAll(alpha, body) => renameAndDropForAlls(body, s.added(alpha, uniqueTypeVariable()))
        case tau: Type => s(tau)
    }

    class Context(items: Map[Variable, TypeScheme]) {
        lazy val freeTypeVariables = items.values.flatMap(Typer.this.freeTypeVariables _).toSet

        def get(variable: Variable): Option[TypeScheme] = items.get(variable)

        def added(variable: Variable, sigma: TypeScheme) = new Context(items + (variable -> sigma))
        def removed(variable: Variable) = new Context(items - variable)
        def updated(variable: Variable, sigma: TypeScheme) = removed(variable).added(variable, sigma)
        def substituted(s: Substitution) = new Context(items.mapValues(s.apply))

        def closureOf(sigma: TypeScheme): TypeScheme = {
            (Typer.this.freeTypeVariables(sigma) -- freeTypeVariables).toList match {
                case Nil => sigma
                case typeVariables => ForAll(typeVariables, sigma)
            }
        }
    }

    private var typeVariableId = 0

    private def uniqueTypeVariable() = {
        typeVariableId += 1
        TypeVariable(s"t${typeVariableId}")
    }
}
