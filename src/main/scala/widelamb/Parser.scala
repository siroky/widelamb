package widelamb

import util.parsing.combinator.{PackratParsers, JavaTokenParsers}
import util.parsing.input.CharSequenceReader

trait Parser extends JavaTokenParsers with PackratParsers {

    def parse(s: String): Either[String, Term] = {
        val phraseParser = phrase(term)
        phraseParser(new CharSequenceReader(s)) match {
            case Success(result, _) => Right(result)
            case NoSuccess(message, _) => Left(message)
        }
    }

    private lazy val term: PackratParser[Term] =
        finalTerm |||
        application

    private lazy val finalTerm: PackratParser[Term] =
        ("(" ~> term <~ ")") |
        abstraction |
        let |
        fix |
        variable |
        integerConstant

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

    private lazy val integerConstant =
        wholeNumber ^^ { n => IntegerConstant(n.toInt) }
}
