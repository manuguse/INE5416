object Kojun {
  type Board = List[List[Option[Int]]]

  var enableDebug = false

  def debugPrint(text: String): Unit = {
    if (enableDebug) println(text) else ()
  }

  def boardSize(board: Board): Int = {
    board.length
  }

  def solve(board: Board, blocks: Board): Option[Board] = {
    debugPrint(s"Resolvendo tabuleiro $board")
    solveBoard(board, blocks)
  }

   def solveBoard(board: Board, blocks: Board): Option[Board] = {
    debugPrint(s"solveBoard chamado com board: $board")
    if (!verify(board, blocks)) {
      debugPrint("Tabuleiro inválido")
      None
    } else if (allFilled(board)) {
      debugPrint("Tabuleiro preenchido corretamente")
      Some(board)
    } else {
      findEmptyCell(board) match {
        case None => debugPrint("Nenhuma célula vazia encontrada") 
                  None
        case Some((r, c)) =>
          debugPrint(s"Tentando preencher célula ($r, $c)")
          val maxValue = maxValueInBlock(blocks, r, c)
          val possibleValues = (1 to maxValue).toList
          val validBoards = possibleValues.collect {
            case v if verify(updateBoard(board, r, c, Some(v)), blocks) =>
              updateBoard(board, r, c, Some(v))
          }
          tryBoards(validBoards, blocks)
      }
    }
  }

    def maxValueInBlock(blocks: Board, r: Int, c: Int): Int = {
    debugPrint(s"Calculando valor máximo para célula ($r, $c)")
    val blockId = blocks(r)(c).getOrElse(-1)
    val blockCells = for {
      i <- blocks.indices
      j <- blocks(i).indices
      if blocks(i)(j).contains(blockId)
    } yield (i, j)
    blockCells.length
  }


  def findEmptyCell(board: Board): Option[(Int, Int)] = {
    val emptyCells = for {
      r <- board.indices
      c <- board(r).indices
      if board(r)(c).isEmpty
    } yield (r, c)
    debugPrint(s"Células vazias encontradas: $emptyCells")
    emptyCells.headOption
  }

  def tryBoards(boards: List[Board], blocks: Board): Option[Board] = {
    boards match {
      case Nil => debugPrint("Nenhum tabuleiro válido encontrado"); None
      case b :: bs =>
        debugPrint(s"Tentando resolver tabuleiro: $b")
        solveBoard(b, blocks) match {
          case Some(solved) => debugPrint("Tabuleiro resolvido"); Some(solved)
          case None => tryBoards(bs, blocks)
        }
    }
  }

  def updateBoard(board: Board, r: Int, c: Int, value: Option[Int]): Board = {
    board.updated(r, board(r).updated(c, value))
  }

  def allFilled(board: Board): Boolean = {
    debugPrint(s"Verificando se todas as células estão preenchidas: $board")
    board.flatten.forall(_.isDefined)
  }

  def verify(board: Board, blocks: Board): Boolean = {
    val result = 
      maxNumEqualsLenBlocks(board, blocks) &&
      noRepeatedNumberInBlock(board, blocks) &&
      noRepeatedNumberInAdjacentCells(board) &&
      noSmallerNumberOnTopOfBiggerNumber(board, blocks)
    debugPrint(s"Resultado da verificação: $result")
    result
  }

  def maxNumEqualsLenBlocks(board: Board, blocks: Board): Boolean = {
    val boardSize = board.length
    val maxBlockValue = board.flatten.flatten.max
    val result = maxBlockValue <= boardSize
    debugPrint(s"Número da célula permitido: $result")
    result
  }

  def uniqueElements(blocks: Board): List[List[(Int, Int)]] = {
    val uniqueValues = blocks.flatten.flatten.distinct
    uniqueValues.map { value =>
      (for {
        r <- blocks.indices
        c <- blocks(r).indices
        if blocks(r)(c).contains(value)
      } yield (r, c)).toList
    }
  }

  def noRepeatedNumberInBlock(board: Board, blocks: Board): Boolean = {
    val uniqueBlocks = uniqueElements(blocks)
    val result = uniqueBlocks.forall { block =>
      val values = block.flatMap { case (r, c) => board(r)(c) }
      values.distinct.length == values.length
    }
    debugPrint(s"Não há números repetidos: $result")
    result
  }

  def noRepeatedNumberInAdjacentCells(board: Board): Boolean = {
    val result = board.indices.forall { r =>
      board(r).indices.forall { c =>
        val cellValue = board(r)(c)
        val adjacentPositions = List((r-1, c), (r+1, c), (r, c-1), (r, c+1))
        val adjacentValues = adjacentPositions.collect {
          case (r, c) if r >= 0 && r < board.length && c >= 0 && c < board(r).length => board(r)(c)
        }.flatten
        cellValue.isEmpty || !adjacentValues.contains(cellValue.get)
      }
    }
    debugPrint(s"Não há células adjacentes com valores iguais: $result")
    result
  }

  def noSmallerNumberOnTopOfBiggerNumber(board: Board, blocks: Board): Boolean = {
    val result = board.indices.forall { i =>
      board(i).indices.forall { j =>
        val currentValue = board(i)(j)
        val aboveValue = if (i > 0 && blocks(i)(j) == blocks(i - 1)(j)) board(i - 1)(j) else None
        (currentValue, aboveValue) match {
          case (Some(curr), Some(above)) => curr <= above
          case _ => true
        }
      }
    }
    debugPrint(s"Não há menores em cima de maiores no mesmo bloco: $result")
    result
  }
}