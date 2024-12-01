object Kojun {
  type Board = List[List[Option[Int]]]

  var enableDebug = true

  def debugPrint(text: String): Unit = {
    if (enableDebug) println(text) else ()
  }

  def boardSize(board: Board): Int = {
    board.length
  }

  def mockBoard(): Board = {
    List(
      List(Some(2), Some(2), Some(3)),
      List(Some(2), Some(3), Some(1)),
      List(Some(3), Some(1), Some(2))
    )
  }

  def solve(board: Board, blocks: Board): Option[Board] = {
    verify(mockBoard(), blocks)
    Some(mockBoard())
  }

  def verify(board: Board, blocks: Board): Boolean = {
    val result = maxNumEqualsLenBlocks(board, blocks) &&
      noRepeatedNumberInBlock(board, blocks) &&
      noRepeatedNumberInAdjacentCells(board) &&
      noSmallerNumberOnTopOfBiggerNumber(board, blocks)
    println(s"Resultado da verificação: $result")
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
    debugPrint(s"Verificando números menores em cima de maiores no mesmo bloco: $result")
    result
  }
}