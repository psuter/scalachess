package chess

import scala.util.Random

// Zobrist hashes of distinct situations are ridiculously unlikely to be equal.
object Zobrist {
  lazy val standardInitial : Long = hash(Game(Variant.Standard).situation)

  def hash(situation: Situation): Long = {
    val Situation(board, color) = situation

    var h: Long = 0L

    for(p <- board.pieces; (pos, piece) = p) {
      h ^= pieceBits(piece.code * 64 + pos.code) 
    }

    h ^= castleBits(board.history.castles.code)

    // TODO add bits for possible en passant files. Would probably need to add that information
    // to Situation? Currently it looks like the only way to know is to try moves for all pawns?

    situation.color.fold(h, h ^ blackToPlay)
  }

  private val pieceBits   : Array[Long] = Array.fill(64 * 12)(Random.nextLong())
  private val castleBits  : Array[Long] = Array.fill(16)(Random.nextLong())
  private val blackToPlay : Long = Random.nextLong()
}
