package org.pigsaw.contactsparsing

import scala.util.parsing.combinator.RegexParsers

/**
  * Created by Nik on 24 Nov 2015.
  */
class ContactsParser extends RegexParsers {

  override val skipWhitespace = false

  val dQuote = "\""
  val anythingWithoutDQuotes = "[^\"]*".r
  val anythingWithoutCommaOrDQuoteOrNewline = """[^,"\n\r]*""".r

  def simpleField = anythingWithoutCommaOrDQuoteOrNewline

  def quotedField = dQuote ~> anythingWithoutDQuotes <~ dQuote

  def field = quotedField | simpleField

  def row = repsep(field, ",")

  def file = repsep(row, """[\n\r]+""".r)
}
