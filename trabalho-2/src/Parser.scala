import scala.io.Source
import scala.util.Try
import Main.Board

object Parser {

  // transforma uma linha do arquivo em uma lista de Option[Int]
  def parseLine(line: String): List[Option[Int]] = {
    line.split("\\s+").toList.map { // separa a linha em palavras e mapeia cada uma delas
      case "-" => None // se a palavra for "-", retorna None
      case x   => Try(Some(x.toInt)).getOrElse(None) // tenta converter a palavra para Int, se não conseguir, retorna None
    }
  }

  // lê o conteúdo do arquivo e converte para uma matriz de Option[Int]
  def parseFileToMatrix(filePath: String): Board = {
    // lê o conteúdo do arquivo e converte em uma lista de linhas
    // ao chamar o parseLine para cada linha, obtemos uma lista de listas de Option[Int]
    val content = Source.fromFile(filePath).getLines().toList
    content.map(parseLine)
  }

  // mostra o tabuleiro em formato mais legivel
  def printBoard(board: Board): Unit = {
    board.foreach { row => // para cada linha do tabuleiro
      row.foreach { cell => // para cada célula da linha
        print(cell.map(_.toString).getOrElse("◻") + " ")
      } // imprime o valor da célula ou "◻" se for None
      println()
    }
  }
}