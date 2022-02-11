/*
 * Copyright 2013 http4s.org
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.http4s
package headers

import cats.data.Ior
import cats.data.NonEmptyList
import cats.parse.Parser
import cats.syntax.all._
import org.http4s.util.Renderable
import org.http4s.util.Writer
import org.typelevel.ci._

object Cookie {
  def apply(head: RequestCookie, tail: RequestCookie*): `Cookie` =
    apply(NonEmptyList(head, tail.toList))

  private def parseCookie(s: String): ParseResult[Cookie] =
    ParseResult.fromParser(parser, "Invalid Cookie header")(s)

  def parseWithWarnings(s: String): Ior[NonEmptyList[ParseFailure], Cookie] =
    parser2
      .parseAll(s)
      .map(
        _.nonEmptyPartition(identity)
          .leftMap(_.map(invalidCookie => ParseFailure("Invalid Cookie", invalidCookie.value)))
      )
      .fold(
        error =>
          Ior.left(
            NonEmptyList.one(
              ParseFailure(
                "Invalid Cookie Header",
                s"Failed to parse `$s` at offset ${error.failedAtOffset}",
              )
            )
          ),
        identity,
      )
      .map(Cookie(_))

  def parse(s: String): Either[ParseFailure, Cookie] = {
    def oneFailure(errors: NonEmptyList[ParseFailure]) = ParseFailure(
      "Invalid Cookie header",
      s"No valid cookies could be parsed: ${errors.map(_.toString).toList.mkString("[", ",", "]")}",
    )

    parseWithWarnings(s).leftMap(oneFailure).fold(Left(_), Right(_), (l, _) => Left(l))
  }

  private[http4s] val parser: Parser[Cookie] = RequestCookie.parser.map(Cookie(_))

  val name: CIString = ci"Cookie"

  implicit val headerInstance: Header[Cookie, Header.Recurring] =
    Header.createRendered(
      name,
      h =>
        new Renderable {
          def render(writer: Writer): writer.type =
            writer.addNel(h.values, sep = "; ")
        },
      parse,
      parseWithWarnings,
    )

  implicit val headerSemigroupInstance: cats.Semigroup[Cookie] =
    (a, b) => Cookie(a.values.concatNel(b.values))

  case class InvalidCookie(value: String) extends AnyVal
  private[http4s] val parser2: Parser[NonEmptyList[Either[InvalidCookie, RequestCookie]]] = {
    import Parser.{char, string}

    val cookieReadErrorParser = Parser.charsWhile(_ != ';').map(InvalidCookie)
    val requestCookieOrErrorParser: Parser[Either[InvalidCookie, RequestCookie]] =
      RequestCookie.parser.eitherOr(cookieReadErrorParser)

    /* cookie-string = cookie-pair *( ";" SP cookie-pair ) */
    val cookieString =
      (requestCookieOrErrorParser ~ (string("; ") *> requestCookieOrErrorParser).rep0).map {
        case (head, tail) =>
          NonEmptyList(head, tail)
      }

    /* We also see trailing semi-colons in the wild, and grudgingly tolerate them
     * here. */
    cookieString <* char(';').?
  }
}

final case class Cookie(values: NonEmptyList[RequestCookie])
