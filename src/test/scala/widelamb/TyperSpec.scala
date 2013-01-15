package widelamb

import org.scalatest.matchers.ShouldMatchers
import org.scalatest.FlatSpec

class TyperSpec extends Parser with Typer with FlatSpec with ShouldMatchers
{
    "The Typer" should "type integer literals" in {
        typing("""0""") shouldMatch { case Integer => true }
        typing("""123""") shouldMatch { case Integer => true }
    }

    it should "type lambda abstraction" in {
        typing("""\x . 1""") shouldMatch {
            case Function(_: TypeVariable, Integer) => true
        }
        typing("""\x . x""") shouldMatch {
            case Function(t1: TypeVariable, t2: TypeVariable) if t1 == t2 => true
        }
        typing("""\x . \y . 123""") shouldMatch {
            case Function(t1: TypeVariable, Function(t2: TypeVariable, Integer)) if t1 != t2 => true
        }
        typing("""\x . \x . 123""") shouldMatch {
            case Function(t1: TypeVariable, Function(t2: TypeVariable, Integer)) if t1 != t2 => true
        }
    }

    private def typing(s: String): (Term, Type) = {
        parse(s).right.flatMap(term => computeType(term).right.map(tau => (term, tau))) match {
            case Right(result) => result
            case Left(message) => fail(message)
        }
    }

    implicit class TypeMatcher(target: (Term, Type)) {
        val term = target._1
        val tau = target._2

        def shouldMatch(matcher: PartialFunction[TypeScheme, Boolean]) {
            if (matcher.isDefinedAt(tau) && matcher(tau)) {
                success()
            } else {
                fail(s"Term $term with type $tau doesn't match the required type.")
            }
        }
    }
}
