object Main {
  type Board = List[List[Option[Int]]]
  import Parser._
  import Kojun._

  def getAvailableBoards(): List[Int] = {
    List(
  1 to 110: _*)
  }

  def main(args: Array[String]): Unit = {
    val availableBoards = getAvailableBoards()
    val boardsToProcess = if (args.isEmpty || !args(0).forall(_.isDigit)) {
      availableBoards
    } else {
        val boardNumber = args(0).toInt
        if (availableBoards.contains(boardNumber)) {
        List(boardNumber)
      } else {
        println(s"Erro: O tabuleiro $boardNumber não está disponível. Os tabuleiros disponíveis são: ${availableBoards.mkString(", ")}")
        return
      }
    }

    boardsToProcess.foreach { number =>
      println("\n--------------------------------\n")
      val boardPath = s"boards/$number.txt"
      val blockPath = s"blocks/$number.txt"

      val board = parseFileToMatrix(boardPath)
      val blocks = parseFileToMatrix(blockPath)

      println(s"Tabuleiro inicial ($number):")
      printBoard(board)

      solve(board, blocks) match {
        case Some(solution) =>
          println("\nSolução:")
          printBoard(solution)
        case None =>
          println("Não foi possível encontrar uma solução.")
      }
    }
  }
}