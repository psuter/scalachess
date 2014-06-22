package chess
package openings

import scala.io.Source

object Openings {
  def main(args : Array[String]) {

    println("Hello!")

    val ob = book
    val graph = ob.graph

    val sic1 = List("e4", "c5", "Nf3", "Nc6", "d4", "cxd4", "Nxd4", "Qc7", "g3")
    val sic2 = List("e4", "c5", "Nc3", "Nc6")

    println(ob.lookup(sic1))
    println(ob.gLookup(sic1).mkString("\n"))

    println("---")

    println(ob.lookup(sic2))
    println(ob.gLookup(sic2).mkString("\n"))
  }

  lazy val book : OpeningBook = {
    OpeningBook.fromECOFile("/home/psuter/git/github/ornicar/scalachess/openings.eco")
  }
}
