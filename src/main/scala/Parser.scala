import fastparse._

import scala.collection.immutable.NumericRange

object Parser {
  import all._
  implicit def chars(r: Range.Inclusive): NumericRange.Inclusive[Char] = r.start.toChar to r.end.toChar

  val S = " "

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
    0x10000 to 0xefff
  ).map(x => P(CharIn(x))).:+(P(CharIn(":_"))).reduce(_ | _)

  val NameChar = NameStartChar | P(CharIn("-.")) | P(CharIn('0' to '9'))
  //#xB7 | [#x0300-#x036F] | [#x203F-#x2040]

  val Name = NameStartChar ~ NameChar.rep
  val startTag = P("<" ~ ">")


}
