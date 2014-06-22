package chess
package openings

import scala.io.Source

import format.pgn.Dumper
import format.pgn.Parser.MoveParser

case class Opening(eco: String, name: String, moves: List[String]) {
  val length: Int = moves.length
}

class OpeningBook(val openings: Seq[(String,String,String)]) {
  // Pairs of (ECO, fullname)
  private val pairs: IndexedSeq[(String,String)] = openings.map(t => (t._1, t._2)).toSet.toIndexedSeq
  private val pairIDs: Map[(String,String),Int] = pairs.zipWithIndex.toMap

  val lineCount   : Int = openings.size 
  val openingCount: Int = pairs.size

  private val emptyBranch = Branch(Map.empty, None)
  private case class Branch(children: Map[String,Branch], opening: Option[Int]) {
    def get(move: String): Option[Branch] = children.get(move)

    def insert(ms: List[String], o: Int): Branch = ms match {
      case Nil => opening match {
        case None => copy(opening = Some(o))
        case Some(o1) if o1 == o => this
        case Some(o2) => this // Duplicate opening! Go fix your book!
      }

      case x :: xs =>
        val newChild = children.getOrElse(x, emptyBranch).insert(xs, o)
        copy(children = children.updated(x, newChild))
    }
  }

  private val tree: Branch = openings.foldLeft(emptyBranch) { (b,t) =>
    b.insert(t._3.split(" ").toList.map(_.trim), pairIDs((t._1, t._2)))
  }

  // Looks up exact move sequence only.
  def lookup(moves: Seq[String]): Option[Opening] = {
    def next(d: Int, b: Branch, ms: List[String], last: Option[(Int,Int)]): Option[(Int,Int)] = {
      val withBranch: Option[(Int,Int)] = b.opening.map(id => (id,d)) orElse last

      ms match {
        case Nil => withBranch
        case x :: xs => b.children.get(x).fold(withBranch) { sb =>
          next(d + 1, sb, xs, withBranch)
        }
      }
    }

    val moveList = moves.toList

    next(0, tree, moveList, None) map { case (id,length) =>
      val (eco,nme) = pairs(id)
      Opening(eco, nme, moveList.take(length))
    } 
  }

  def gLookup(moves: Seq[String]): List[(Int,String)] = {
    def next(d: Int, h: Long, ms: List[String], seen: List[(Int,Int)]) : List[(Int,Int)] = {
      val here : GraphNode = graph.get(h).get
      val withNode = here.openings.toList.map(o => (d,o)) ++ seen

      ms match {
        case Nil => withNode
        case x :: xs => graph.followFrom(h, x).fold(withNode) { nh =>
          next(d + 1, nh, xs, withNode)
        }
      }
    }

    next(0, Zobrist.standardInitial, moves.toList, Nil) map { p =>
      val (eco,nme) = pairs(p._2)
      (p._1, s"$eco $nme")
    }
  }

  case class GraphNode(situation: Situation, openings: Set[Int])
  type OpeningGraph = SituationGraph[String,GraphNode]
  lazy val graph: OpeningGraph = {
    def addBranch(graph: OpeningGraph, hash: Long, node: GraphNode, branch: Branch): OpeningGraph = {
      // Add situation to the graph, generate all successor positions based on 
      // b.children (they should all be possible and match the Strings), continue
      // recursively, hope the stack doesn't explode.
      
      val g = branch.opening.fold(graph) { o =>
        graph.withNode(hash, node.copy(openings = node.openings + o))
      }

      val s = node.situation
      
      branch.children.foldLeft(g) { (g: OpeningGraph, mb: (String,Branch)) =>
        val (sm, sb) = mb
        
        g.followFrom(hash, sm) map { (sh: Long) =>
          addBranch(g, sh, g.get(sh).get, sb)
        } getOrElse {
          // The edge did not exist yet. We compute the position after the move...
          sitMove(s, sm) map { (ss: Situation) =>
            val sh: Long = Zobrist.hash(ss)
            // Maybe check for Zobrist collisions here, if we're worried about these things.
            val sn: GraphNode = g.get(sh).getOrElse(GraphNode(ss, Set.empty))
            val withEdge = g.withNode(sh, sn).withEdge(hash, sh, sm)
            addBranch(withEdge, sh, sn, sb)
          } getOrElse {
            // println("Failed to apply move: " + sm + " to situation: " + s)
            g
          }
        }
      }
    }

    // Adds all possible edges between known positions.
    def connect(graph: OpeningGraph, grow: Boolean): OpeningGraph = {
      var nec: Int = 0
      var nnc: Int = 0
      val ret = graph.nodes.foldLeft(graph) { (g,hn) =>
        val (h,n) = hn
        val s = n.situation
        val knownMoves: Set[String] = g.outEdges(h).getOrElse(Nil).toSet
        val allMoves: Set[(Move,String)] = s.moves.toList.flatMap(_._2).toSet.map((m:Move) => (m,Dumper(m)))
        val newMoves = allMoves.filter(p => !knownMoves(p._2))

        newMoves.foldLeft(g) { (g,mp) => 
          val ns = mp._1.situationAfter
          val nh = Zobrist.hash(ns)
          if(g.isDefinedAt(nh)) {
            nec += 1
            g.withEdge(h, nh, mp._2)
          } else if(grow) {
            nec += 1
            nnc += 1
            g.withNode(nh, GraphNode(ns, Set.empty)).withEdge(h, nh, mp._2)
          } else {
            g
          }
        }
      }
      println(s"$nec new edges, $nnc new nodes")
      ret
    }


    val n = GraphNode(Situation(Variant.Standard), Set.empty)
    val h = Zobrist.hash(n.situation)
    val g: OpeningGraph = SituationGraph.empty.withNode(h, n)


    // Equivalent to tree only:
   
    val g1 = time("Graph generation") {
      addBranch(g, h, n, tree)
    }

    val g2 = time("Connection") {
      connect(g1, false)
    }

    g2
  }

  private def time[T](desc: String)(op: =>T) : T = {
    val t0 = System.currentTimeMillis
    val v = op
    val t1 = System.currentTimeMillis
    println(s"$desc : ${(t1 - t0) / 1000.0}")
    v
  }

  // TODO FIXME: this could be useful elsewhere, maybe move?
  private def sitMove(situation: Situation, move: String): Valid[Situation] = for {
    san ← MoveParser(move)
    move ← san(Game(situation))
  } yield move.situationAfter
}

object OpeningBook {
  def fromECOFile(fileName: String): OpeningBook = {
    val src = Source.fromFile(fileName)
    
    val openings = src.getLines flatMap { line =>
      val bits = line.split(";").map(_.trim)

      if(bits.size == 3) {
        Some((bits(0), bits(1), bits(2)))
      } else {
        None
      }
    } toSeq

    new OpeningBook(openings)
  }
}
