package org.pigsaw.contactsparsing

import scala.util.parsing.combinator.RegexParsers

/**
  * Created by Nik on 24 Nov 2015.
  */
class ContactsParser extends RegexParsers {

  val dQuote = "\""
  val anythingWithoutDQuotes = "[^\"]*".r
  val anythingWithoutCommaOrDQuote = """[^,"]*""".r

  def simpleField = anythingWithoutCommaOrDQuote

  def quotedField = dQuote ~> anythingWithoutDQuotes <~ dQuote

  def field = quotedField | simpleField
}
