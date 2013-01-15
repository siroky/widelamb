package widelamb

abstract class Substitution {
    def apply(sigma: TypeScheme): TypeScheme
    def apply(tau: Type): Type
    def compose(that: Substitution): Substitution = new ComposedSubstitution(this, that)
}

class SimpleSubstitution(map: Map[TypeVariable, Type]) extends Substitution {
    def apply(sigma: TypeScheme): TypeScheme = sigma match {
        case ForAll(alpha, body) => ForAll(alpha, apply(body))
        case tau: Type => apply(tau)
    }

    def apply(tau: Type): Type = tau match {
        case Function(domain, range) => Function(apply(domain), apply(range))
        case alpha: TypeVariable => map.withDefault(x => x)(alpha)
        case _ => tau
    }

    def added(alpha: TypeVariable, tau: Type) = new SimpleSubstitution(map + (alpha -> tau))
}

object IdentitySubstitution extends SimpleSubstitution(Map.empty)

class ComposedSubstitution(first: Substitution, second: Substitution) extends Substitution {
    def apply(sigma: TypeScheme): TypeScheme = first(second(sigma))
    def apply(sigma: Type): Type = first(second(sigma))
}
