package chess

import scala.math.{ min, max, abs }

class Pos private(val z: Byte) extends AnyVal {
  import Pos.posAt

  def x: Int = 1 + (z % 8)
  def y: Int = 1 + (z / 8)

  def up: Option[Pos] = posAt(x, y + 1)
  def down: Option[Pos] = posAt(x, y - 1)
  def right: Option[Pos] = posAt(x + 1, y)
  def left: Option[Pos] = posAt(x - 1, y)
  def upLeft: Option[Pos] = up flatMap (_ left)
  def upRight: Option[Pos] = up flatMap (_ right)
  def downLeft: Option[Pos] = down flatMap (_ left)
  def downRight: Option[Pos] = down flatMap (_ right)

  def >|(stop: Pos => Boolean): List[Pos] = |<>|(stop, _.right)
  def |<(stop: Pos => Boolean): List[Pos] = |<>|(stop, _.left)
  def |<>|(stop: Pos => Boolean, dir: Direction): List[Pos] = dir(this) map { p =>
    p :: (if (stop(p)) Nil else p.|<>|(stop, dir))
  } getOrElse Nil

  def ?<(other: Pos): Boolean = x < other.x
  def ?>(other: Pos): Boolean = x > other.x
  def ?+(other: Pos): Boolean = y < other.y
  def ?^(other: Pos): Boolean = y > other.y
  def ?|(other: Pos): Boolean = x == other.x
  def ?-(other: Pos): Boolean = y == other.y

  def <->(other: Pos): Iterable[Pos] =
    min(x, other.x) to max(x, other.x) map { posAt(_, y) } flatten

  def touches(other: Pos): Boolean = xDist(other) <= 1 && yDist(other) <= 1

  def onSameDiagonal(other: Pos): Boolean = xDist(other) == yDist(other)
  def onSameLine(other: Pos): Boolean = ?-(other) || ?|(other)

  def xDist(other: Pos) = abs(x - other.x)
  def yDist(other: Pos) = abs(y - other.y)

  def file = Pos xToString x
  def rank = y.toString
  def key = file + rank
  def color = Color((x % 2 == 0) ^ (y % 2 == 0))

  def piotr = Pos piotrChars z
  def piotrStr = piotr.toString

  override def toString = key
}

object Pos {

  def posAt(x: Int, y: Int): Option[Pos] = allCoords get (x, y)

  def posAt(key: String): Option[Pos] = allKeys get key

  def xToString(x: Int) = (96 + x).toChar.toString

  def piotr(c: Char): Option[Pos] = allPiotrs get c

  def keyToPiotr(key: String) = posAt(key) map (_.piotr)
  def doubleKeyToPiotr(key: String) = for {
    a ← keyToPiotr(key take 2)
    b ← keyToPiotr(key drop 2)
  } yield s"$a$b"
  def doublePiotrToKey(piotrs: String) = for {
    a ← piotr(piotrs.head)
    b ← piotr(piotrs(1))
  } yield s"${a.key}${b.key}"

  private val piotrChars : Array[Char] = Array(
    'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h',
    'i', 'j', 'k', 'l', 'm', 'n', 'o', 'p',
    'q', 'r', 's', 't', 'u', 'v', 'w', 'x',
    'y', 'z', 'A', 'B', 'C', 'D', 'E', 'F',
    'G', 'H', 'I', 'J', 'K', 'L', 'M', 'N',
    'O', 'P', 'Q', 'R', 'S', 'T', 'U', 'V',
    'W', 'X', 'Y', 'Z', '0', '1', '2', '3',
    '4', '5', '6', '7', '8', '9', '!', '?'
  )

  val A1 = new Pos(0)
  val B1 = new Pos(1)
  val C1 = new Pos(2)
  val D1 = new Pos(3)
  val E1 = new Pos(4)
  val F1 = new Pos(5)
  val G1 = new Pos(6)
  val H1 = new Pos(7)
  val A2 = new Pos(8)
  val B2 = new Pos(9)
  val C2 = new Pos(10)
  val D2 = new Pos(11)
  val E2 = new Pos(12)
  val F2 = new Pos(13)
  val G2 = new Pos(14)
  val H2 = new Pos(15)
  val A3 = new Pos(16)
  val B3 = new Pos(17)
  val C3 = new Pos(18)
  val D3 = new Pos(19)
  val E3 = new Pos(20)
  val F3 = new Pos(21)
  val G3 = new Pos(22)
  val H3 = new Pos(23)
  val A4 = new Pos(24)
  val B4 = new Pos(25)
  val C4 = new Pos(26)
  val D4 = new Pos(27)
  val E4 = new Pos(28)
  val F4 = new Pos(29)
  val G4 = new Pos(30)
  val H4 = new Pos(31)
  val A5 = new Pos(32)
  val B5 = new Pos(33)
  val C5 = new Pos(34)
  val D5 = new Pos(35)
  val E5 = new Pos(36)
  val F5 = new Pos(37)
  val G5 = new Pos(38)
  val H5 = new Pos(39)
  val A6 = new Pos(40)
  val B6 = new Pos(41)
  val C6 = new Pos(42)
  val D6 = new Pos(43)
  val E6 = new Pos(44)
  val F6 = new Pos(45)
  val G6 = new Pos(46)
  val H6 = new Pos(47)
  val A7 = new Pos(48)
  val B7 = new Pos(49)
  val C7 = new Pos(50)
  val D7 = new Pos(51)
  val E7 = new Pos(52)
  val F7 = new Pos(53)
  val G7 = new Pos(54)
  val H7 = new Pos(55)
  val A8 = new Pos(56)
  val B8 = new Pos(57)
  val C8 = new Pos(58)
  val D8 = new Pos(59)
  val E8 = new Pos(60)
  val F8 = new Pos(61)
  val G8 = new Pos(62)
  val H8 = new Pos(63)

  val all = List(A1, B1, C1, D1, E1, F1, G1, H1, A2, B2, C2, D2, E2, F2, G2, H2, A3, B3, C3, D3, E3, F3, G3, H3, A4, B4, C4, D4, E4, F4, G4, H4, A5, B5, C5, D5, E5, F5, G5, H5, A6, B6, C6, D6, E6, F6, G6, H6, A7, B7, C7, D7, E7, F7, G7, H7, A8, B8, C8, D8, E8, F8, G8, H8)

  val allKeys: Map[String, Pos] = all map { pos => pos.key -> pos } toMap

  val allPiotrs: Map[Char, Pos] = all map { pos => pos.piotr -> pos } toMap

  val allCoords: Map[(Int, Int), Pos] = all map { pos => (pos.x, pos.y) -> pos } toMap
}
