package widelamb

object Main extends App with Parser with Typer with JavaScriptBackEnd {

    //val source = """(fix sum . \x . ifzero x 0 (plus x (sum (minus x 1)))) 10"""
    //val source = """(fix gcd . \a . \b . ifzero b a (gcd b (mod a b))) 1071 462"""
    //val source = """(fix fact . \x . ifzero x 1 (mul x (fact (minus x 1)))) 10"""
    val source = """let x = (\x . x) in (x mul) (x 3) (x 4)"""

    val output = parse(source).right.flatMap(typeTerm _) match {
        case Right((term, tau)) => {
            if (tau == Integer) {
                generateOutput(term)
            } else {
                s"The program result is of a function type $tau."
            }
        }
        case Left(message) => message
    }

    println(output)
}
