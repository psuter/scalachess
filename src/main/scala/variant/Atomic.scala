package chess
package variant

case object Atomic extends Variant(
  id = 7,
  key = "atomic",
  name = "Atomic",
  shortName = "Atom",
  title = "Nuke your opponent's king to win.",
  standardInitialPosition = true) {

  override def hasMoveEffects = true

  /** Move threatens to explode the opponent's king */
  private def explodesOpponentKing(situation: Situation)(move: Move): Boolean = move.captures && {
    situation.board.kingPosOf(!situation.color) exists (_.surroundingPositions contains move.dest)
  }

  /** Move threatens to illegally explode our own king */
  private def explodesOwnKing(situation: Situation)(move: Move): Boolean = {
    move.captures && (situation.kingPos map (_.surroundingPositions contains (move.dest)) getOrElse false)
  }

  private def protectedByOtherKing(board: Board, to: Pos, color: Color): Boolean =
    board.kingPosOf(color) map (_.surroundingPositions.contains(to)) getOrElse false

  /**
   * In atomic chess, a king cannot be threatened while it is in the perimeter of the other king as were the other player
   * to capture it, their own king would explode. This effectively makes a king invincible while connected with another
   * king.
   */
  override def kingThreatened(board: Board, color: Color, to: Pos, filter: Piece => Boolean = _ => true): Boolean = {
    board.pieces exists {
      case (pos, piece) if piece.color == color && filter(piece) && piece.eyes(pos, to) && !protectedByOtherKing(board, to, color) =>
        (!piece.role.projection) || piece.role.dir(pos, to).exists {
          longRangeThreatens(board, pos, _, to)
        }
      case _ => false
    }
  }

  // moves exploding opponent king are always playable
  override def kingSafety(m: Move, filter: Piece => Boolean, kingPos: Option[Pos]): Boolean = {
    !kingPos.exists(kingThreatened(m.after, !m.color, _, filter)) ||
      explodesOpponentKing(m.situationBefore)(m)
  } && !explodesOwnKing(m.situationBefore)(m)

  /** If the move captures, we explode the surrounding pieces. Otherwise, nothing explodes. */
  private def explodeSurroundingPieces(move: Move): Move = {
    if (move.captures) {
      val surroundingPositions = move.dest.surroundingPositions
      val afterBoard = move.after
      val destination = move.dest

      val boardPieces = afterBoard.pieces

      // Pawns are immune (for some reason), but all pieces surrounding the captured piece and the capturing piece
      // itself explode
      val piecesToExplode = surroundingPositions.filter(boardPieces.get(_).fold(false)(_.isNot(Pawn))) + destination
      val afterExplosions = boardPieces -- piecesToExplode

      val newBoard = afterBoard withPieces afterExplosions
      move withAfter newBoard
    }
    else move
  }

  override def addVariantEffect(move: Move): Move = explodeSurroundingPieces(move)

  /**
   * Since a king may walk into the path of another king, it is more difficult to win when your opponent only has a
   * king left.
   */
  private def insufficientAtomicWinningMaterial(board: Board) = {
    val whiteActors = board.actorsOf(White)
    val blackActors = board.actorsOf(Black)
    val allActors = board.actors
    lazy val allPieces = board.actors.values.map(_.piece).filter(_ isNot King)

    // One player must only have their king left
    if (whiteActors.size != 1 && blackActors.size != 1) false
    else {
      // You can mate with a queen or a pawn that is later promoted a a queen, but not with just one of any other piece
      // or with just a king
      allPieces.size == 1 && !allPieces.exists(p => p.is(Queen) || p.is(Pawn)) || allActors.size == 2
    }
  }

  override def specialDraw(situation: Situation) = {
    // Bishops on opposite coloured squares can never capture each other to cause a king to explode and a traditional
    // mate would be not be
    val board = situation.board
    InsufficientMatingMaterial.bishopsOnDifferentColor(board) || insufficientAtomicWinningMaterial(board)
  }

  // On insufficient mating material, a win may still be commonly achieved by exploding a piece next to a king
  override def drawsOnInsufficientMaterial = false

  /** Atomic chess has a special end where a king has been killed by exploding with an adjacent captured piece */
  override def specialEnd(situation: Situation) = situation.board.kingPos.size != 2
}

