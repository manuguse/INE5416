import scala.io.Source
import scala.util.Try
import Main.Board

object Parser {

  // transforma uma linha do arquivo em uma lista de Option[Int]
  def parseLine(line: String): List[Option[Int]] = {
    line.split("\\s+").toList.map {
      case "-" => None
      case x   => Try(Some(x.toInt)).getOrElse(None)
    }
  }

  // lê o conteúdo do arquivo e converte para uma matriz de Option[Int]
  def parseFileToMatrix(filePath: String): Board = {
    val content = Source.fromFile(filePath).getLines().toList
    content.map(parseLine)
  }

  // associa cada valor único de block a uma cor ANSI
  def getColorMap(block: Board): Map[Int, String] = {
    val colors: Seq[String] = Seq(
      "\u001b[31m", // vermelho
      "\u001b[32m", // verde
      "\u001b[33m", // amarelo
      "\u001b[34m", // azul
      "\u001b[35m", // roxo
      "\u001b[36m", // ciano
      "\u001b[91m", // vermelho claro
      "\u001b[92m", // verde claro
      "\u001b[93m", // amarelo claro
      "\u001b[94m", // azul claro
      "\u001b[95m", // roxo claro
      "\u001b[96m", // ciano claro
      "\u001b[90m", // cinza
      "\u001b[37m", // branco
      "\u001b[97m"  // branco brilhante
    )
    val uniqueValues = block.flatten.flatten.distinct
    uniqueValues.zip(colors.cycle.toIterable).toMap
  }

  // mostra o tabuleiro colorido baseado no block
  def printBoard(board: Board, block: Board): Unit = {
    val reset = "\u001b[0m"
    val colorMap = getColorMap(block)

    board.zip(block).foreach { case (row, blockRow) =>
      row.zip(blockRow).foreach {
        case (Some(value), Some(blockValue)) =>
          val color = colorMap.getOrElse(blockValue, "")
          print(s"$color$value$reset ")
        case (None, Some(blockValue)) =>
          val color = colorMap.getOrElse(blockValue, "")
          print(s"$color◻$reset ")
        case (None, None) =>
          print("◻ ")
        case (Some(_), None) => ???
      }
      println()
    }
  }

  implicit class CycleSeq[T](seq: Seq[T]) {
    def cycle: Iterator[T] = Iterator.continually(seq).flatten
  }
}