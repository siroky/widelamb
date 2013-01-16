package widelamb

import org.scalatest.matchers.ShouldMatchers
import org.scalatest.FlatSpec

class TyperSpec extends Parser with Typer with FlatSpec with ShouldMatchers
{
    val integerTemplate: PartialFunction[Type, Boolean] = { case Integer => true }
    val identityFunctionTemplate: PartialFunction[Type, Boolean] = {
        case Function(t1: TypeVariable, t2: TypeVariable) if t1 == t2 => true
    }
    
    "The Typer" should "type integer literals" in {
        typing("""0""") shouldMatch integerTemplate
        typing("""123""") shouldMatch integerTemplate
    }

    it should "type lambda abstraction" in {
        typing("""plus""") shouldMatch { case Function(Integer, Function(Integer, Integer)) => true }
        typing("""\x . 1""") shouldMatch { case Function(_: TypeVariable, Integer) => true }
        typing("""\x . x""") shouldMatch identityFunctionTemplate
        typing("""\x . \y . 123""") shouldMatch {
            case Function(t1: TypeVariable, Function(t2: TypeVariable, Integer)) if t1 != t2 => true
        }
        typing("""\x . \x . 123""") shouldMatch {
            case Function(t1: TypeVariable, Function(t2: TypeVariable, Integer)) if t1 != t2 => true
        }
    }

    it should "type lambda application" in {
        typing("""plus 1 2""") shouldMatch integerTemplate
        typing("""plus (\x . x)""") shouldMatch integerTemplate
        typing("""plus 1""") shouldMatch { case Function(Integer, Integer) => true }
        typing("""(\x . x) 1""") shouldMatch integerTemplate
        typing("""(\x . x) (\x . x)""") shouldMatch identityFunctionTemplate
        typing("""(\x . ifzero x 1 (mul x (mul (minus x 1) 2))) 10""") shouldMatch integerTemplate
    }

    it should "type let" in {
        typing("""let x = (\x . x) in x""") shouldMatch identityFunctionTemplate
        typing("""let x = (\x . x) in (x mul) (x 1) (x 2)""") shouldMatch integerTemplate
    }

    def typing(s: String): (Term, Type) = {
        parse(s).right.flatMap(term => computeType(term).right.map(tau => (term, tau))) match {
            case Right(result) => result
            case Left(message) => fail(message)
        }
    }

    implicit class TypeMatcher(target: (Term, Type)) {
        val term = target._1
        val tau = target._2

        def shouldMatch(matcher: PartialFunction[Type, Boolean]) {
            if (matcher.isDefinedAt(tau) && matcher(tau)) {
                success()
            } else {
                fail(s"Term $term with type $tau doesn't match the required type.")
            }
        }
    }
}
