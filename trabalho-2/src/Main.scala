object Main {
  type Board = List[List[Option[Int]]]
  import Parser._
  import Kojun._

  def getAvailableBoards(): List[Int] = {
    List(0, 1, 8, 14)
  }

  def main(args: Array[String]): Unit = {
    val availableBoards = getAvailableBoards()
    val boardsToProcess = if (args.isEmpty) availableBoards else List(args(0).toInt)

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