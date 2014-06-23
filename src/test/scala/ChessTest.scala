package chess

import format.Visual
import org.specs2.matcher.Matcher
import org.specs2.mutable.Specification
import ornicar.scalalib.test.ValidationMatchers
import scalaz.{ Validation => V }

trait ChessTest
    extends Specification
    with ValidationMatchers {

  implicit def stringToBoard(str: String): Board = Visual << str

  class BoardBuilder(str: String) {
    def chess960: Board = makeBoard(str, Variant.Chess960)
  }
  implicit def stringToBoardBuilder(str: String) = new BoardBuilder(str)

  class SituationBuilder(str: String) {
    def as(color: Color): Situation = Situation(Visual << str, color)
  }
  implicit def stringToSituationBuilder(str: String) = new SituationBuilder(str)

  class RichActor(actor: Actor) {
    def threatens(to: Pos): Boolean = actor.piece.role match {
      case x: Projection => x.dir(actor.pos, to) exists { Actor.longRangeThreatens(actor.board, actor.pos, _, to) }
      case _             => actor.piece.eyes(actor.pos, to)
    }
  }
  implicit def enrichActor(actor: Actor) = new RichActor(actor)

  class RichGame(game: Game) {
    def as(color: Color): Game = game.copy(player = color)

    def playMoves(moves: (Pos, Pos)*): Valid[Game] = playMoveList(moves)

    def playMoveList(moves: Iterable[(Pos, Pos)]): Valid[Game] = {
      val vg = moves.foldLeft(V.success(game): Valid[Game]) { (vg, move) =>
        // vg foreach { x =>
          // println(s"------------------------ ${x.turns} = $move")
        // }
        // because possible moves are asked for player highlight
        // before the move is played (on initial situation)
        vg foreach { _.situation.destinations }
        val ng = vg flatMap { g => g(move._1, move._2) map (_._1) }
        ng
      }
      // vg foreach { x => println("========= PGN: " + x.pgnMoves) }
      vg
    }

    def playMove(
      orig: Pos,
      dest: Pos,
      promotion: Option[PromotableRole] = None): Valid[Game] =
      game.apply(orig, dest, promotion) map (_._1)

    def withClock(c: Clock) = game.copy(clock = Some(c))
  }
  implicit def enrichGame(game: Game) = new RichGame(game)

  def makeBoard(pieces: (Pos, Piece)*): Board =
    Board(pieces toMap, History(), Variant.Standard)

  def makeBoard(str: String, variant: Variant) =
    Visual << str withVariant variant

  def makeBoard: Board = Board init Variant.Standard

  def makeEmptyBoard: Board = Board empty Variant.Standard

  def bePoss(poss: Pos*): Matcher[Option[Iterable[Pos]]] = beSome.like {
    case p => sortPoss(p.toList) must_== sortPoss(poss.toList)
  }

  def makeGame: Game = Game(makeBoard)

  def bePoss(board: Board, visual: String): Matcher[Option[Iterable[Pos]]] = beSome.like {
    case p => Visual.addNewLines(Visual.>>|(board, Map(p -> 'x'))) must_== visual
  }

  def beBoard(visual: String): Matcher[Valid[Board]] = beSuccess.like {
    case b => b.visual must_== (Visual << visual).visual
  }

  def beSituation(visual: String): Matcher[Valid[Situation]] = beSuccess.like {
    case s => s.board.visual must_== (Visual << visual).visual
  }

  def beGame(visual: String): Matcher[Valid[Game]] = beSuccess.like {
    case g => g.board.visual must_== (Visual << visual).visual
  }

  def sortPoss(poss: Seq[Pos]): Seq[Pos] = poss sortBy (_.toString)

  def pieceMoves(piece: Piece, pos: Pos): Option[List[Pos]] =
    (makeEmptyBoard place piece at pos).toOption flatMap { b =>
      b actorAt pos map (_.destinations)
    }
}
