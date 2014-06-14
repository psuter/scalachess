package chess

import scalaz.Success
import format.pgn._

import scala.collection.mutable.{ HashMap => MutableHashMap }

/* This object uses the opening DB to generate a finite state machine that can tag move sequences.
 * The FSM can detect openings (main lines), and to some extent transpositions into openings. */
object OpeningDag {

  case class Opening(code: String, name: String, length: Int)
  case class TransposedOpening(code: String, name: String, atPly: Int)

  trait OpeningTagger {
    def tag(moves: List[String]) : (Option[Opening], List[TransposedOpening])
  }

  // How many plies the DAG generator should venture out of the known lines
  // to try and detect transpositions.
  private val explorePlyCount : Int = 2

  case class Info(set: Set[(Int,Int,Boolean)])

  type OpeningGraph = SituationGraph[String,Info]


  def main(args : Array[String]) {
    val g = graph
    println("The graph has " + g.nodeCount + " nodes.")
  }

  lazy val openings : Array[Opening] = Openings.db.toArray.map { case (code, name, moves) =>
    Opening(code, name, moves.split(' ').length)
  }

  lazy val graph : OpeningGraph = {
    var g : OpeningGraph = SituationGraph.empty.withNode(Zobrist.standardInitial, Info(Set.empty))

    // Side-effecting !
    def storeOpeningSequence(id: Int, code: String, name: String, moves: String) {
      Parser(moves) foreach { parsed =>
        val length    = moves.split(' ').length

        var game      : Game      = Game(Variant.Standard) 
        var situation : Situation = game.situation
        var hash      : Long      = Zobrist.hash(situation)

        for((san, plyCount) <- parsed.sans.zipWithIndex) {
          val move: Valid[Move] = san(game)

          move match {
            case Success(m) =>
              val moveStr      = Dumper(m.situationBefore, m, m.situationAfter)

              game = game(m)
              val newSituation = game.situation
              val newHash      = Zobrist.hash(newSituation)

              val triple: (Int,Int,Boolean) = (id, (plyCount + 1), (plyCount + 1) == length)

              val newInfo : Info = g.get(newHash).map { i =>
                Info(i.set + triple)
              } getOrElse {
                Info(Set(triple))
              }

              g = g.withNode(newHash, newInfo).withEdge(hash, newHash, moveStr)
              
              situation = newSituation
              hash      = newHash

            case _ =>
              println("There was an error.")
              return
          } 
        }
      }
    }

    for((triple, oid) <- Openings.db.zipWithIndex; (code, name, moves) = triple) {
      storeOpeningSequence(oid, code, name, moves)
    }

    g
  }

  lazy val tagger: OpeningTagger = {
    val g : OpeningGraph = graph

    new OpeningTagger {
      def tag(moves: List[String]): (Option[Opening], List[TransposedOpening]) = {
        var h = Zobrist.standardInitial
        var continue : Boolean = true

        var longestMatch : Int = -1

        var plyCount : Int = 0
        var followed : Set[Int] = Set.empty

        for(m <- moves if continue) {
          plyCount += 1

          g.followFrom(h, m) match {
            case Some(nh) =>
              val sets = g.get(nh).map(_.set).getOrElse(Set.empty)
              val matchingPlyCount : Set[Int] = sets.filter(t => t._2 == plyCount).map(_._1)

              if(plyCount == 1) {
                followed = matchingPlyCount
              } else {
                followed = followed intersect matchingPlyCount
              }

              val finalFollowed = sets.filter(t => followed(t._1) && t._3)
              if(!finalFollowed.isEmpty) {
                longestMatch = finalFollowed.head._1
              }

              h = nh

            case None =>
              continue = false
          }
        }

        val opening = if(longestMatch >= 0) Some(openings(longestMatch)) else None

        (opening, Nil)
      }
    }
  }
}
