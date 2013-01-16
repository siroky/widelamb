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
            Variable("mod") -> Function(List(Integer, Integer), Integer),
            Variable("mul") -> Function(List(Integer, Integer), Integer)
        )
    )

    def typeTerm(term: Term): Either[String, (Term, Type)] = {
        typeTerm(initialContext, term).right.map(result => (term, result._2))
    }

    def typeTerm(context: Context, term: Term): Result = term match {
        case literal: Literal => Right(Substitution.identity, literal.tpe)
        case variable: Variable => typeVariable(context, variable)
        case application: Application => typeApplication(context, application)
        case abstraction: Abstraction => typeAbstraction(context, abstraction)
        case let: Let => typeLet(context, let)
        case fix: Fix => typeFix(context, fix)
    }

    def typeVariable(context: Context, variable: Variable): Result = {
        context.get(variable) match {
            case Some(sigma) => Right(Substitution.identity, renameAndDropForAlls(sigma))
            case None => Left(s"Can't find type of the free variable '$variable'.")
        }
    }

    def typeApplication(context: Context, application: Application): Result = {
        typeTerm(context, application.function).right.flatMap { case (s1, tau1) =>
            typeTerm(s1(context), application.argument).right.flatMap { case (s2, tau2) =>
                val beta = uniqueTypeVariable()
                mostGeneralUnifier(s2(tau1), Function(tau2, beta)) match {
                    case Some(unifier) => Right(unifier compose s2 compose s1, unifier(beta))
                    case None => Left(
                        s"Can't find type of term $application. The argument type $tau2 doesn't match the function " +
                        s"signature ${s2(tau1)}."
                    )
                }
            }
        }
    }

    def typeAbstraction(context: Context, abstraction: Abstraction): Result = {
        val alpha = uniqueTypeVariable()
        val updatedContext = context.updated(abstraction.variable, alpha)
        typeTerm(updatedContext, abstraction.body).right.flatMap { case (s1, tau1) =>
            Right(s1, Function(s1(alpha), tau1))
        }
    }

    def typeLet(context: Context, let: Let): Result = {
        typeTerm(context, let.value).right.flatMap { case (s1, tau1) =>
            val substitutedContext = s1(context)
            val variableScheme = substitutedContext.closureOf(tau1)
            val updatedContext = substitutedContext.updated(let.variable, variableScheme)
            typeTerm(updatedContext, let.body).right.flatMap { case (s2, tau2) =>
                Right(s2 compose s1, tau2)
            }
        }
    }

    def typeFix(context: Context, fix: Fix): Result = {

        def computationLoop(context: Context, sigma: TypeScheme, substitutions: List[Substitution]): Result = {
            typeTerm(context, fix.body).right.flatMap { case (s, tau) =>
                val substitutedContext = s(context)
                val nextSigma = substitutedContext.closureOf(tau)
                val nextContext = substitutedContext.updated(fix.variable, nextSigma)
                val nextSubstitutions = s :: substitutions
                if (s(sigma) == nextSigma) {
                    Right(nextSubstitutions.reverse.reduce(_ compose _), tau)
                } else {
                    computationLoop(nextContext, nextSigma, nextSubstitutions)
                }
            }
        }

        val alpha = uniqueTypeVariable()
        val initialSigma = ForAll(alpha, alpha)
        val initialContext = context.updated(fix.variable, initialSigma)
        computationLoop(initialContext, initialSigma, Nil)
    }

    def mostGeneralUnifier(tau1: Type, tau2: Type): Option[Substitution] = {
        mostGeneralUnifier(List(tau1 -> tau2), Substitution.identity)
    }

    /**
     * Most general unification algorithm as described in the Handbook of Automated Reasoning, Franz Baader and Wayne
     * Snyder, 2001 (http://www.cs.bu.edu/~snyder/publications/UnifChapter.pdf).
     */
    def mostGeneralUnifier(equations: List[(Type, Type)], s: Substitution): Option[Substitution] = equations match {
        case Nil => Some(s)
        case equation :: rest => equation match {
            case (tau1, tau2) if tau1 == tau2 => {
                mostGeneralUnifier(rest, s)
            }
            case (Function(d1, r1), Function(d2, r2)) => {
                mostGeneralUnifier(d1 -> d2 :: r1 -> r2 :: rest, s)
            }
            case (tau, alpha: TypeVariable) if !tau.isInstanceOf[TypeVariable] => {
                mostGeneralUnifier(alpha -> tau :: rest, s)
            }
            case (alpha: TypeVariable, tau) if !freeTypeVariables(tau)(alpha) => {
                val s1 = Substitution.identity.added(alpha, tau)
                val updatedRest = rest.map(e => s1(e._1) -> s1(e._2))
                mostGeneralUnifier(updatedRest, s1 compose s)
            }
            case _ => None
        }
    }

    def freeTypeVariables(sigma: TypeScheme): Set[TypeVariable] = sigma match {
        case ForAll(variable, body) => freeTypeVariables(body) - variable
        case Function(domain, range) => freeTypeVariables(domain) union freeTypeVariables(range)
        case variable: TypeVariable => Set(variable)
        case _ => Set.empty
    }

    def renameAndDropForAlls(sigma: TypeScheme, s: Substitution = Substitution.identity): Type = sigma match {
        case ForAll(alpha, body) => renameAndDropForAlls(body, s.added(alpha, uniqueTypeVariable()))
        case tau: Type => s(tau)
    }

    class Context(val items: Map[Variable, TypeScheme]) {
        lazy val freeTypeVariables = items.values.flatMap(Typer.this.freeTypeVariables _).toSet

        def get(variable: Variable): Option[TypeScheme] = items.get(variable)
        def added(variable: Variable, sigma: TypeScheme) = new Context(items + (variable -> sigma))
        def removed(variable: Variable) = new Context(items - variable)
        def updated(variable: Variable, sigma: TypeScheme) = removed(variable).added(variable, sigma)

        def closureOf(sigma: TypeScheme): TypeScheme = {
            (Typer.this.freeTypeVariables(sigma) -- freeTypeVariables).toList match {
                case Nil => sigma
                case typeVariables => ForAll(typeVariables, sigma)
            }
        }
    }

    class Substitution(val bindings: Map[TypeVariable, Type]) {
        def apply(context: Context): Context = {
            new Context(context.items.mapValues(apply))
        }
        
        def apply(sigma: TypeScheme): TypeScheme = sigma match {
            case ForAll(alpha, body) => ForAll(alpha, apply(body))
            case tau: Type => apply(tau)
        }

        def apply(tau: Type): Type = tau match {
            case Function(domain, range) => Function(apply(domain), apply(range))
            case alpha: TypeVariable => bindings.withDefault(x => x)(alpha)
            case _ => tau
        }

        def added(alpha: TypeVariable, tau: Type) = new Substitution(bindings + (alpha -> tau))

        def compose(that: Substitution): Substitution = {
            val composedBindings = bindings.mapValues(that.apply)
            val missingBindings = that.bindings -- bindings.keys
            new Substitution(composedBindings ++ missingBindings)
        }
    }

    object Substitution extends {
        val identity = new Substitution(Map.empty)
    }

    private var typeVariableId = 0

    private def uniqueTypeVariable() = {
        typeVariableId += 1
        TypeVariable(s"t${typeVariableId}")
    }
}
