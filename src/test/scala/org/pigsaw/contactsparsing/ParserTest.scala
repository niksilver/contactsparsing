package org.pigsaw.contactsparsing

import org.scalatest.{ShouldMatchers, FunSpec}

import scala.util.Success
import scala.util.parsing.combinator.Parsers

/**
  * Created by Nik on 24 Nov 2015.
  */
class ParserTest extends FunSpec with ShouldMatchers {

  trait TestParser extends ContactsParser {
    def parseOption[T](parser: Parser[T], input: String): Option[T] =
      parseAll(parser, input) match {
        case Success(out, _) => Some(out)
        case fail => None
      }
  }

  describe("simpleField") {
    it ("should know a simple string is a field") {
      new TestParser {
        parseOption(simpleField, "hello") should equal(Some("hello"))
        parseOption(simpleField, "byeee") should equal(Some("byeee"))
      }
    }

    it ("should accept empty string as a field") {
      new TestParser {
        parseOption(simpleField, "") should equal (Some(""))
      }
    }

    it ("should accept a field with dots and spaces and alphanumerics") {
      new TestParser {
        parseOption(simpleField, "dot... dot... dot...") should equal (Some("dot... dot... dot..."))
      }
    }

    it ("should reject a string with a comma") {
      new TestParser {
        parseOption(simpleField, "aye,aye") should equal (None)
      }
    }

    it ("should reject a string with a double quote") {
      new TestParser {
        parseOption(simpleField, """aye "!" aye""") should equal (None)
      }
    }
  }

  describe("quotedField") {
    it ("should accept quoted content with spaces") {
      new TestParser {
        parseOption(quotedField, "\"The cat sat on the mat\"") should equal (Some("The cat sat on the mat"))
        parseOption(quotedField, "\"The rain in Spain...\"") should equal (Some("The rain in Spain..."))
      }
    }

    it ("should accept quoted content with newlines") {
      new TestParser {
        val multiline =
          """One, two,
            |three, four,
            |five, six.
          """.stripMargin
        parseOption(quotedField, "\"" + multiline + "\"") should equal (Some(multiline))
      }
    }

    it ("should reject multiline content with double quotes") {
      new TestParser {
        val multiline =
          """One, two,
            |three, "four,
            |five, six.
          """.stripMargin
        parseOption(quotedField, "\"" + multiline + "\"") should equal (None)
      }
    }
  }
}
