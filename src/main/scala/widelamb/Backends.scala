package widelamb

/**
 * A back end capable of producing runnable program in particular language corresponding to the input lambda term.
 */
trait BackEnd {
    def generateOutput(term: Term): String
}

/**
 * A JavaScript backend. Function parameters aren't evaluated lazily in JavaScript, so they must to be wrapped into
 * parameterless anonymous functions. Consequently, all fix and let terms have to be wrapped in the same fashion,
 * because when they're used in their body, they're indistinguishable from the function parameters.
 */
trait JavaScriptBackEnd extends BackEnd {

    /** The bootstrap code needed to execute the programs. */
    val bootstrap =
        """|
           |Function.prototype.curried = function(params, args) {
           |    var f = this;
           |    args = args || [];
           |    return function() {
           |        if (params === 0) {
           |            return f.apply(null, args);
           |        }
           |        return function(arg) {
           |            return f.curried(params - 1, args.concat([arg]));
           |        };
           |    };
           |};
           |
           |var ifzero = function(x, then, otherwise) { return x() === 0 ? then() : otherwise(); }.curried(3);
           |var plus = function(a, b) { return a() + b(); }.curried(2);
           |var minus = function(a, b) { return a() - b(); }.curried(2);
           |var mul = function(a, b) { return a() * b(); }.curried(2);
           |var div = function(a, b) { return Math.floor(a() / b()); }.curried(2);
           |var mod = function(a, b) { return a() % b(); }.curried(2);
        """.stripMargin

    /** Produces a JavaScript program corresponding to the specified term. */
    def generateOutput(term: Term): String = bootstrap +
        s"""|
            |var program = ${process(term)};
            |alert(program());
        """.stripMargin

    /**
     * Returns an expression corresponding to the specified term. The expression represents a computation that the term
     * is performing, so it must be evaluated in order to get it's value. It may be perceived as a lazy value of the
     * term.
     */
    private def process(term: Term): String = term match {
        case Variable(name) => name
        case Application(function, value) => s"${evaluate(function)}(function() { return ${evaluate(value)}; })"
        case Let(variable, value, body) => {
            s"(function() { var ${variable.name} = ${process(value)}; return ${process(body)}; }())"
        }
        case Fix(variable, body) => {
            s"(function ${variable.name}() { return ${process(body)}(); })"
        }
        case _ => lazify(evaluate(term))
    }

    private def lazify(expr: Any) = s"function() { return $expr; }"

    private def evaluate(term: Term): String = term match {
        case IntegerConstant(value) => value.toString
        case Abstraction(variable, body) => s"(function(${variable.name}) { return ${process(body)}; })"
        case _ => s"${process(term)}()"
    }
}
