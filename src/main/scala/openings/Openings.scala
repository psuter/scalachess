package chess
package openings

import scala.io.Source

object Openings {
  def main(args : Array[String]) {
    val ob = book
    val graph = ob.graph

    println(ob.gLookup(List("e3", "e6", "e4", "e5")).mkString("\n"))
  }

  lazy val book : OpeningBook = {
    OpeningBook.fromECOFile("/home/psuter/git/github/ornicar/scalachess/openings.eco")
  }
}
