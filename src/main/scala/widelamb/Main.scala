package widelamb

object Main extends App with Compiler with JavaScriptBackEnd {
    if (args.isEmpty) {
        println("""You must specify the program to compile.""")
        println("""Usage within SBT: run "(fix fact . \\x . ifzero x 1 (mul x (fact (minus x 1)))) 10"""")
    } else {
        println(compile(args.head))
    }
}
