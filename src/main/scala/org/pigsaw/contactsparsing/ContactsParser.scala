package org.pigsaw.contactsparsing

import java.io.PrintWriter

import scala.io.Source
import scala.util.parsing.combinator.RegexParsers

/**
  * Created by Nik on 24 Nov 2015.
  */
class ContactsParser extends RegexParsers {

  override val skipWhitespace = false

  val dQuote = "\""
  val anythingWithoutCommaOrDQuoteOrNewline = """[^,"\n\r]*""".r
  val anythingWithoutIsolatedQuotes = """((\"\")|[^\"])*""".r

  def simpleField = anythingWithoutCommaOrDQuoteOrNewline

  def quotedField = dQuote ~> anythingWithoutIsolatedQuotes <~ dQuote ^^
    { _.replaceAll("[\n\r]+", ", ").replaceAll("\"\"", "\"") }

  def field = quotedField | simpleField

  def row = repsep(field, ",")

  def file = repsep(row, """[\n\r]+""".r)

  def output(str: String): String = parseAll(file, str) match {
    case Success(out, _) => flatten(out)
    case failure: NoSuccess => "Oops: " + failure
  }

  def flatten(out: List[List[String]]): String = {
    def quoted(f: String) = "\"" + f + "\""
    def toRow(li: List[String]) = li.filter(_.nonEmpty).map(quoted).mkString(",")
    (out map toRow).mkString(System.lineSeparator)
  }
}

object ContactsParser {
  def main(args: Array[String]): Unit = {
    val parser = new ContactsParser
    val content = Source.fromFile("google.csv", "UTF-16").mkString
    val out = parser.output(content)

    val pw = new PrintWriter("google-out.csv")
    pw.print(out)
    pw.close()
  }
}