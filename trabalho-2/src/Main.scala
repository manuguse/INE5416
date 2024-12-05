object Main {
  type Board = List[List[Option[Int]]]
  import Parser._
  import Kojun._

  def getAvailableBoards(): List[Int] = {
    List(1 to 35: _*) // pode ir até 110, mas botando aqui pra facilitar a visualização
  }

  def main(args: Array[String]): Unit = {
    val availableBoards = getAvailableBoards() // pega os 110 tabuleiros disponíveis (a função existe mais porque antes eu nao tinha todos)
    val boardsToProcess = if (args.isEmpty || !args(0).forall(_.isDigit)) { // se não passar argumento ou se o argumento não for um número
      availableBoards // vamos usar todos os tabuleiros
    } else { // se houver argumento, vamos usar apenas o tabuleiro passado
        val boardNumber = args(0).toInt
        if (availableBoards.contains(boardNumber)) {
        List(boardNumber) // criamos uma lista com só com o número, pra fazer a iteração
      } else {
        println(s"Erro: O tabuleiro $boardNumber não está disponível. Os tabuleiros disponíveis são: ${availableBoards.mkString(", ")}")
        return
      }
    }

    boardsToProcess.foreach { number => // para cada número (tabuleiro)
      println("\n--------------------------------\n")
      val boardPath = s"boards/$number.txt" // pegamos o path
      val blockPath = s"blocks/$number.txt" // pegamos o path

      val board = parseFileToMatrix(boardPath) // parseia para uma matriz
      val blocks = parseFileToMatrix(blockPath) // parseia para uma matriz

      println(s"Tabuleiro inicial ($number):")
      printBoard(board, blocks)

      solve(board, blocks) match { // tenta resolver
        case Some(solution) => // se tiver solução imprime o tabuleiro
          println("\nSolução:")
          printBoard(solution, blocks)
        case None => // se não tiver solução avisa que não foi possível encontrar
          println("Não foi possível encontrar uma solução.")
      }
    }
  }
}