import scala.io.Source
import scala.util.Try
import Main.Board

object Parser {

  // Transforma uma linha do arquivo em uma lista de Option[Int]
  def parseLine(line: String): List[Option[Int]] = {
    line.split("\\s+").toList.map {
      case "-" => None
      case x   => Try(Some(x.toInt)).getOrElse(None)
    }
  }

  // Lê o conteúdo do arquivo e converte para uma matriz de Option[Int]
  def parseFileToMatrix(filePath: String): Board = {
    val content = Source.fromFile(filePath).getLines().toList
    content.map(parseLine)
  }

  // Imprime o tabuleiro em formato legível
  def printBoard(board: Board): Unit = {
    board.foreach { row =>
      row.foreach { cell =>
        print(cell.map(_.toString).getOrElse("◻") + " ")
      }
      println()
    }
  }
}