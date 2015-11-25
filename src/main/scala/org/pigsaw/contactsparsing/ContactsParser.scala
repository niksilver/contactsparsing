package org.pigsaw.contactsparsing

import scala.util.parsing.combinator.RegexParsers

/**
  * Created by Nik on 24 Nov 2015.
  */
class ContactsParser extends RegexParsers {

  def simpleField = """[^,"]*""".r

  def quotedField = "\"" ~> "[^\"]*".r <~ "\""
}
