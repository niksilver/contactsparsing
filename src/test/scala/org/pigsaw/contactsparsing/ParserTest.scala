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

    it ("should reject a string with a newline") {
      new TestParser {
        val multiline =
          """one
            |two
            |three""".stripMargin
        parseOption(simpleField, multiline) should equal (None)
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

    it ("should accept quoted content with newlines and convert newlines to comma-spaces") {
      new TestParser {
        val multiline =
          """One, two,
            |three, four,
            |five, six.""".stripMargin
        parseOption(quotedField, "\"" + multiline + "\"") should equal (Some("One, two,, three, four,, five, six."))
      }
    }

    it ("should accept quoted content with double-double quotes") {
      new TestParser {
        val text = """Here "" is a double-double quote"""
        parseOption(quotedField, "\"" + text + "\"") should equal (Some("Here \"\" is a double-double quote"))
      }
    }

    it ("should accept quoted content with double-double quotes at the start") {
      new TestParser {
        val text = "\"\"Here is a double-double quote"
        parseOption(quotedField, "\"" + text + "\"") should equal (Some("\"\"Here is a double-double quote"))
      }
    }

    it ("should accept quoted content with double-double quotes at the end") {
      new TestParser {
        val text = "Here is a double-double quote\"\""
        parseOption(quotedField, "\"" + text + "\"") should equal (Some("Here is a double-double quote\"\""))
      }
    }

    it ("should accept quoted content which is just two double-double quotes") {
      new TestParser {
        val text = "\"\"\"\""
        parseOption(quotedField, "\"" + text + "\"") should equal (Some("\"\"\"\""))
      }
    }

    it ("should reject quoted content which is just an odd number of double quotes") {
      new TestParser {
        val text = "\"\"\"\"\""
        parseOption(quotedField, "\"" + text + "\"") should equal (None)
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

  describe("field") {
    it ("should accept a simple field") {
      new TestParser {
        parseOption(field, "hello") should equal(Some("hello"))
        parseOption(field, "byeee") should equal(Some("byeee"))
      }
    }

    it ("should accept a quoted field") {
      new TestParser {
        parseOption(field, "\"The cat sat on the mat\"") should equal (Some("The cat sat on the mat"))
        parseOption(field, "\"The rain in Spain...\"") should equal (Some("The rain in Spain..."))
      }
    }
  }

  describe("row") {
    it ("should accept a mix of fields on a single line") {
      new TestParser {
        parseOption(row, """Once,upon,a time,"a girl",named""") should
          equal (Some(List("Once", "upon", "a time","a girl", "named")))
      }
    }

    it ("should accept a row with a multi-line field") {
      new TestParser {
        val multiline =
          """There was
            |a young man
            |from Dundee""".stripMargin
        parseOption(row, s"""one,"$multiline",three""") should
          equal (Some(List("one", "There was, a young man, from Dundee", "three")))
      }
    }
  }

  describe("file") {
    it ("should parse a file with a single line") {
      new TestParser {
        val content = """one,two,three four,"five six",seven"""
        parseOption(file, content) should equal (Some(
          List(List("one", "two", "three four", "five six", "seven"))
        ))
      }
    }

    it ("should parse a file with multiline fields") {
      new TestParser {
        val content =
          """one,two,three four,"Some numbers
            |go over lines",six,seven
            |a b c,d e f""".stripMargin
        val brokenField = "Some numbers, go over lines"
        parseOption(file, content) should equal (Some(
          List(List("one", "two", "three four", brokenField, "six", "seven"),
            List("a b c", "d e f"))
        ))
      }
    }
  }

  describe("output") {
    it ("should output file contents as a flattened string") {
      new TestParser {
        val content =
          """one,two,three four,"Some numbers
            |go over lines",six,seven
            |a b c,d e f""".stripMargin
        val brokenField = "Some numbers, go over lines"
        output(content) should equal (
          """"one","two","three four","Some numbers, go over lines","six","seven"
             |"a b c","d e f"""".stripMargin)
      }
    }
  }
}
