package widelamb

/**
 * A configurable compiler depending on a particular back end.
 */
trait Compiler extends Parser with Typer { self: BackEnd =>

    def compile(input: String): String = {
        parse(input).right.flatMap(typeTerm) match {
            case Right((term, tau)) => {
                if (tau == Integer) {
                    generateOutput(term)
                } else {
                    s"The program is of a function type $tau."
                }
            }
            case Left(message) => message
        }
    }
}
