package widelamb

trait BackEnd {
    def generateOutput(term: Term): String
}

trait JavaScriptBackEnd extends BackEnd {
    def generateOutput(term: Term): String =
        s"""
            Function.prototype.curried = function(params, args) {
                var f = this;
                args = args || [];
                return function() {
                    if (params === 0) {
                        return f.apply(null, args);
                    }
                    return function(arg) {
                        return f.curried(params - 1, args.concat([arg]));
                    };
                };
            };

            var ifzero = function(x, then, otherwise) { return x() === 0 ? then() : otherwise(); }.curried(3);
            var plus = function(a, b) { return a() + b(); }.curried(2);
            var minus = function(a, b) { return a() - b(); }.curried(2);
            var mul = function(a, b) { return a() * b(); }.curried(2);
            var div = function(a, b) { return Math.floor(a() / b()); }.curried(2);
            var mod = function(a, b) { return a() % b(); }.curried(2);

            alert(${process(term)}());
        """

    def process(term: Term): String = term match {
        case IntegerLiteral(value) => lazyExpression(value)
        case Variable(name) => name
        case Application(function, value) => {
            s"${process(function)}()(function() { return ${process(value)}(); })"
        }
        case Abstraction(variable, body) => {
            lazyExpression(s"(function(${variable.name}) { return ${process(body)}; })")
        }
        case Let(variable, value, body) => {
            s"(function() { var ${variable.name} = ${process(value)}; return ${process(body)}; }())"
        }
        case Fix(variable, body) => {
            s"(function ${variable.name}() { return ${process(body)}(); })"
        }
    }

    def lazyExpression(expression: Any): String = s"function() { return $expression; }"
}
