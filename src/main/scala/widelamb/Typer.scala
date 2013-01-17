package widelamb

/**
 * A Hindley-Milner type checker. Further description at http://en.wikipedia.org/wiki/Hindley–Milner
 */
trait Typer {

    /** Type of the algorithm result. Either an error message or a pair of substitution and inferred type. */
    type Result = Either[String, (Substitution, Type)]

    /** The initial gamma context with built-in integral functions. */
    val initialContext = new Context(
        Map[Variable, TypeScheme](
            Variable("ifzero") -> usingNewTypeVariable(a => ForAll(a, Function(List(Integer, a, a), a))),
            Variable("plus") -> Function(List(Integer, Integer), Integer),
            Variable("minus") -> Function(List(Integer, Integer), Integer),
            Variable("div") -> Function(List(Integer, Integer), Integer),
            Variable("mul") -> Function(List(Integer, Integer), Integer)
        )
    )

    /** Returns either an error message or the input term together with its type. */
    def typeTerm(term: Term): Either[String, (Term, Type)] = {
        typeTerm(initialContext, term).right.map(result => (term, result._2))
    }

    /** Infers type of any term. */
    def typeTerm(context: Context, term: Term): Result = term match {
        case literal: Constant => Right(Substitution.identity, literal.tau)
        case variable: Variable => typeVariable(context, variable)
        case application: Application => typeApplication(context, application)
        case abstraction: Abstraction => typeAbstraction(context, abstraction)
        case let: Let => typeLet(context, let)
        case fix: Fix => typeFix(context, fix)
    }

    /** Infers type of a type variable. */
    def typeVariable(context: Context, variable: Variable): Result = {
        context.get(variable) match {
            case Some(sigma) => {
                /**
                 * Given a type scheme ForEach alpha . M, substitutes all alpha_{i}s in the vector alpha for new
                 * beta_{i}s and Returns M.
                 */
                def toUniqueType(sigma: TypeScheme, s: Substitution = Substitution.identity): Type = sigma match {
                    case ForAll(alpha, body) => toUniqueType(body, s.added(alpha, newTypeVariable()))
                    case tau: Type => s(tau)
                }
                Right(Substitution.identity, toUniqueType(sigma))
            }
            case None => Left(s"Can't find type of the free variable '$variable'.")
        }
    }

    /** Infers type of a lambda application. */
    def typeApplication(context: Context, application: Application): Result = {
        typeTerm(context, application.function).right.flatMap { case (s1, tau1) =>
            typeTerm(s1(context), application.argument).right.flatMap { case (s2, tau2) =>
                val beta = newTypeVariable()
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

    /** Infers type of a lambda abstraction. */
    def typeAbstraction(context: Context, abstraction: Abstraction): Result = {
        val alpha = newTypeVariable()
        val updatedContext = context.updated(abstraction.variable, alpha)
        typeTerm(updatedContext, abstraction.body).right.flatMap { case (s1, tau1) =>
            Right(s1, Function(s1(alpha), tau1))
        }
    }

    /** Infers type of a let term. */
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

    /** Infers type of a fix term. */
    def typeFix(context: Context, fix: Fix): Result = {
        val initialSigma = usingNewTypeVariable(alpha => ForAll(alpha, alpha))
        val initialContext = context.updated(fix.variable, initialSigma)

        /**
         * Given the previous gamma context, type scheme sigma that is > than type of the fix term and a list of
         * substitutions used to get to the sigma, infers more concrete type scheme of the fix term. Recursively calls
         * itself until a fixed point is reached. Composes all the substitutions used to get to the fixed point and
         * returns them together with the inferred type.
         */
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
        computationLoop(initialContext, initialSigma, Nil)
    }

    /** Returns most general unifier of the two specified types. */
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
                // For equal types, the substitution s doesn't have to be altered.
                mostGeneralUnifier(rest, s)
            }
            case (Function(d1, r1), Function(d2, r2)) => {
                // Function decomposition.
                mostGeneralUnifier(d1 -> d2 :: r1 -> r2 :: rest, s)
            }
            case (tau, alpha: TypeVariable) if !tau.isInstanceOf[TypeVariable] => {
                // Ensures that type variable is always the first type in the equation.
                mostGeneralUnifier(alpha -> tau :: rest, s)
            }
            case (alpha: TypeVariable, tau) if !freeTypeVariables(tau)(alpha) => {
                // Replace all occurrences of alpha in the equations with tau in case tha alpha isn't recursive.
                val s1 = Substitution.identity.added(alpha, tau)
                val updatedRest = rest.map(e => s1(e._1) -> s1(e._2))
                mostGeneralUnifier(updatedRest, s1 compose s)
            }
            case _ => None
        }
    }

    /** Returns free type variables in a type scheme. Or all type variables in a type. */
    def freeTypeVariables(sigma: TypeScheme): Set[TypeVariable] = sigma match {
        case ForAll(variable, body) => freeTypeVariables(body) - variable
        case Function(domain, range) => freeTypeVariables(domain) union freeTypeVariables(range)
        case variable: TypeVariable => Set(variable)
        case _ => Set.empty
    }

    /** Representation of the gamma context as a mapping of variables to their type schemes. */
    class Context(val items: Map[Variable, TypeScheme]) {
        lazy val freeTypeVariables = items.values.flatMap(Typer.this.freeTypeVariables _).toSet

        def get(variable: Variable): Option[TypeScheme] = items.get(variable)
        def added(variable: Variable, sigma: TypeScheme) = new Context(items + (variable -> sigma))
        def removed(variable: Variable) = new Context(items - variable)
        def updated(variable: Variable, sigma: TypeScheme) = removed(variable).added(variable, sigma)

        /** Closes all free variables in the type scheme that aren't free in the context. */
        def closureOf(sigma: TypeScheme): TypeScheme = {
            (Typer.this.freeTypeVariables(sigma) -- freeTypeVariables).toList match {
                case Nil => sigma
                case typeVariables => ForAll(typeVariables, sigma)
            }
        }
    }

    /** Substitution as a mapping of type variables to types that should be substituted for them. */
    class Substitution(val bindings: Map[TypeVariable, Type]) {

        /** Applies the substitution on a context. */
        def apply(context: Context): Context = {
            new Context(context.items.mapValues(apply))
        }

        /** Applies the substitution on a type scheme. */
        def apply(sigma: TypeScheme): TypeScheme = sigma match {
            case ForAll(alpha, body) => ForAll(alpha, apply(body))
            case tau: Type => apply(tau)
        }

        /** Applies the substitution on a type. */
        def apply(tau: Type): Type = tau match {
            case Function(domain, range) => Function(apply(domain), apply(range))
            case alpha: TypeVariable => bindings.withDefault(x => x)(alpha)
            case _ => tau
        }

        /** Returns composition of this and the specified substitution, i.e. s(x) = this(that(x)). */
        def compose(that: Substitution): Substitution = {
            val composedBindings = that.bindings.mapValues(apply)
            val missingBindings = bindings -- that.bindings.keys
            new Substitution(composedBindings ++ missingBindings)
        }

        def added(alpha: TypeVariable, tau: Type) = new Substitution(bindings + (alpha -> tau))
    }

    object Substitution extends {
        val identity = new Substitution(Map.empty)
    }

    private var typeVariableId = 0

    private def newTypeVariable() = {
        typeVariableId += 1
        TypeVariable(s"t${typeVariableId}")
    }

    private def usingNewTypeVariable[A](f: TypeVariable => A) = f(newTypeVariable())
}
