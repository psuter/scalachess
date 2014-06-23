package chess
package format.pgn

import Pos._

class ParserTest extends ChessTest {

  import Fixtures._

  val parser = Parser

  "promotion check" should {
    "as a queen" in {
      parser("b8=Q ") must beSuccess.like {
        case a => a.sans.headOption must beSome.like {
          case san: Std => san.promotion must_== Some(Queen)
        }
      }
    }
    "as a rook" in {
      parser("b8=R ") must beSuccess.like {
        case a => a.sans.headOption must beSome.like {
          case san: Std => san.promotion must_== Some(Rook)
        }
      }
    }
  }

  "no tag but inline result" in {
    parser(noTagButResult) must beSuccess.like {
      case ParsedPgn(List(Tag(Tag.Result, result)), _) => result must_== "1-0"
    }
  }

  "nags" in {
    parser(withNag) must beSuccess
  }

  raws foreach { sans =>
    val size = sans.split(' ').size
    "sans only size: " + size in {
      parser(sans) must beSuccess.like {
        case a => a.sans.size must_== size
      }
    }
  }

  (shortCastles ++ longCastles) foreach { sans =>
    val size = sans.split(' ').size
    "sans only size: " + size in {
      parser(sans) must beSuccess.like {
        case a => a.sans.size must_== size
      }
    }
  }

  "disambiguated" in {
    parser(disambiguated) must beSuccess.like {
      case a => a.sans.pp.size must_== 3
    }
  }

  List(fromProd1, fromProd2, castleCheck1, castleCheck2) foreach { sans =>
    val size = sans.split(' ').size
    "sans only from prod size: " + size in {
      parser(sans) must beSuccess.like {
        case a => a.sans.size must_== size
      }
    }
  }

  "variations" in {
    parser(variations) must beSuccess.like {
      case a => a.sans.size must_== 20
    }
  }

  "game from wikipedia" in {
    parser(fromWikipedia) must beSuccess.like {
      case a => a.sans.size must_== 85
    }
  }

  "game from crafty" in {
    parser(fromCrafty) must beSuccess.like {
      case a => a.sans.size must_== 68
    }
  }

  "inline comments" in {
    parser(inlineComments) must beSuccess.like {
      case a => a.sans.size must_== 85
    }
  }

  "comments and variations" in {
    parser(commentsAndVariations) must beSuccess.like {
      case a => a.sans.size must_== 103
    }
  }

  "comments and lines by smartchess" in {
    parser(bySmartChess) must beSuccess.like {
      case a => a.sans.size must_== 65
    }
  }

  "complete 960" in {
    parser(complete960) must beSuccess.like {
      case a => a.sans.size must_== 42
    }
  }

  "TCEC" in {
    parser(fromTcec) must beSuccess.like {
      case a => a.sans.size must_== 142
    }
  }

  "TCEC with engine output" in {
    parser(fromTcecWithEngineOutput) must beSuccess.like {
      case a => a.sans.size must_== 165
    }
  }

  "chesskids iphone" in {
    parser(chesskids) must beSuccess.like {
      case a => a.sans.size must_== 135
    }
  }

  "handwritten" in {
    parser(handwritten) must beSuccess.like {
      case a => a.sans.size must_== 139
    }
  }

  "chess by post" in {
    parser(chessByPost) must beSuccess.like {
      case a => a.sans.size must_== 100
    }
  }

  "Android device" in {
    parser(android) must beSuccess.like {
      case a => a.sans.size must_== 69
    }
  }
}
