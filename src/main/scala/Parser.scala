import fastparse._

import scala.collection.immutable.NumericRange

object Parser extends App {
  import all._
  implicit def chars(r: Range.Inclusive): NumericRange.Inclusive[Char] = r.start.toChar to r.end.toChar

  val S = P(CharIn(Seq(0x20.toChar, 0x9.toChar, 0xD.toChar, 0xA.toChar))).rep
  val space = P(CharIn(List(0x20.toChar)))
  val digit = P(CharIn(0 to 9))
  val hex = List[NumericRange.Inclusive[Char]]('a' to 'z', 'A' to 'Z', 0 to 9).map(x => P(CharIn(x))).reduce(_ | _)

  val NameStartChar = List[NumericRange.Inclusive[Char]](
    'A' to 'Z',
    'a' to 'z',
    0xc0 to 0xd6,
    0xd8 to 0xf6,
    0xf8 to 0x2ff,
    0x370 to 0x37d,
    0x37f to 0x1fff,
    0x200c to 0x200d,
    0x2070 to 0x218f,
    0x2c00 to 0x2fef,
    0x3001 to 0xd7ff,
    0xf900 to 0xfdcf,
    0xfdf0 to 0xfffd,
    0x10000 to 0xeffff
  ).map(x => P(CharIn(x))).:+(P(CharIn(":_"))).reduce(_ | _)

  val NameChar = List[NumericRange.Inclusive[Char]](
    '0' to '9',
    0x0300 to 0x036f,
    0x203f to 0x2040
  ).map(x => P(CharIn(x))).++(List(P(CharIn("-.")), NameStartChar)).reduce(_ | _)


  val Name = NameStartChar ~ NameChar.rep
  val Names = Name ~ (space ~ Name).rep
  val Nmtoken = NameChar.rep
  val Nmtokens = Nmtoken ~ (space ~ Nmtoken).rep


  val CharRef = P("&#" ~ digit ~ digit.rep ~ ";") | P("&#x" ~ hex ~ hex.rep ~ ";")

  val Reference = P(EntityRef | CharRef)
  val EntityRef = P("&" ~ Name.! ~ ";")
  val PEReference = P("%" ~ Name.! ~ ";")





  val EntityValue = P("\"" ~ (P()))


  val startTag = P("<" ~ ">")



}
