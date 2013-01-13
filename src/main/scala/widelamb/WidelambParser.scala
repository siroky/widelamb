package widelamb

import util.parsing.combinator.{PackratParsers, JavaTokenParsers}
import util.parsing.input.CharSequenceReader

class WidelambParser extends JavaTokenParsers with PackratParsers
{
    def parse(s: String): ParseResult[Term] = {
        val phraseParser = phrase(term)
        phraseParser(new CharSequenceReader(s))
    }

    private lazy val term: PackratParser[Term] =
        finalTerm |||
        application

    private lazy val finalTerm: PackratParser[Term] =
        ("(" ~> term <~ ")") |
        fix |
        let |
        abstraction |
        variable |
        integerLiteral

    private lazy val application: PackratParser[Application] =
        term ~ finalTerm ^^ { case f ~ v => Application(f, v) }

    private lazy val abstraction =
        ("\\" ~> variable) ~ ("." ~> term) ^^ { case v ~ t => Abstraction(v, t) }

    private lazy val let =
        ("let" ~> variable) ~ ("=" ~> term) ~ ("in" ~> term) ^^ { case v ~ t ~ b => Let(v, t, b) }

    private lazy val fix =
        ("fix" ~> variable) ~ ("." ~> term) ^^ { case v ~ t => Fix(v, t) }

    private lazy val variable =
        not("let" | "in" | "fix") ~> """[a-zA-Z]+""".r ^^ { i => Variable(i) }

    private lazy val integerLiteral =
        wholeNumber ^^ { n => IntegerLiteral(n.toInt) }
}
