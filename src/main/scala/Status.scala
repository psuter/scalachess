package chess

sealed abstract class Status(val id: Int) extends Ordered[Status] {

  def compare(other: Status) = id compare other.id

  def name = toString

  def is(s: Status): Boolean = this == s

  def is(f: Status.type => Status): Boolean = is(f(Status))
}

object Status {

  case object Created extends Status(10)
  case object Started extends Status(20)
  case object Aborted extends Status(25) // from this point the game is finished
  case object Mate extends Status(30)
  case object Resign extends Status(31)
  case object Stalemate extends Status(32)
  case object Timeout extends Status(33) // when player leaves the game
  case object Draw extends Status(34)
  case object Outoftime extends Status(35) // clock flag
  case object Cheat extends Status(36)
  case object NoStart extends Status(37) // the player did not make the first move in time

  val all = List(Created, Started, Aborted, Mate, Resign, Stalemate, Timeout, Draw, Outoftime, Cheat, NoStart)

  val finishedNotCheated = all filter { s =>
    s.id >= Mate.id && s.id < Cheat.id
  }

  val finishedWithWinner = List(Mate, Resign, Timeout, Outoftime, Cheat, NoStart)

  val byId = all map { v => (v.id, v) } toMap

  def apply(id: Int): Option[Status] = byId get id
}
