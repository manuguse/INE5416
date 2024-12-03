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
      // verifica se repeita as regras do jogo. se não respeitar, retorna None
      if (!verify(board, blocks)) {
          debugPrint("Tabuleiro inválido")
          None
      // se todas as células estão preenchidas (!= None) e válidas, retorna o tabuleiro
      } else if (allFilled(board)) { 
          debugPrint("Tabuleiro preenchido corretamente")
          Some(board)
      } else {
          // tenta achar uma célula vazia. 
          findEmptyCell(board) match {
            // em teoria, não deveria acontecer, pois a função allFilled já verifica
            // se todas as células estão preenchidas, mas é uma verificação extra
              case None => debugPrint("Nenhuma célula vazia encontrada") 
                                  None
              // se encontrar uma célula vazia, tenta preencher com um valor válido
              case Some((row, column)) =>
                  debugPrint(s"Tentando preencher célula ($row, $column)")
                  // define o valor máximo como o tamanho do bloco
                  val maxValue = maxValueInBlock(blocks, row, column)
                  // define os valores possíveis para a célula (de 1 até o valor máximo)
                  val possibleValues = (1 to maxValue).toList
                  // filtra os valores possíveis para a célula, verificando se o tabuleiro
                  // continua válido após a inserção do valor.
                  val validBoards = possibleValues.collect { // o collect é um map + filter
                    // vamos para todos os possíveis valores verificar se o tabuleiro é válido
                    // se for, retorna o tabuleiro com o valor inserido
                      case v if verify(updateBoard(board, row, column, Some(v)), blocks) =>
                          updateBoard(board, row, column, Some(v))
                  }
                  // tenta resolver recursivamente os tabuleiros válidos gerados
                  tryBoards(validBoards, blocks)
          }
      }
  }

  def maxValueInBlock(blocks: Board, row: Int, column: Int): Int = {
      debugPrint(s"Calculando valor máximo para célula ($row, $column)")
      // pegamos o id da célula, ou seja, o bloco ao qual ela pertence
      val blockId = blocks(row)(column).getOrElse(-1)
      // pegamos todas as células do bloco que contém o id
      // o zipWithIndex funciona transformando o array em um array de tuplas, 
      // onde o primeiro elemento é o valor e o segundo é o índice
      val blockCells = blocks.zipWithIndex.flatMap { case (blockRow, i) =>
          blockRow.zipWithIndex.collect { 
              case (cell, j) if cell.contains(blockId) => (i, j)
          }
      }
      blockCells.length
  }

  // procura a primeira célula vazia no tabuleiro
  def findEmptyCell(board: Board): Option[(Int, Int)] = {
    // obtem os índices das linhas e colunas do tabuleiro
      board.indices.flatMap { row => // para cada linha, aplica a função
          board(row).indices.collectFirst { // para cada coluna, retorna o primeiro valor que satisfaz a condição
              case column if board(row)(column).isEmpty => (row, column) // se a célula estiver vazia, retorna a posição
          }
      }.headOption // retorna um option ou com a primeira célula vazia ou None
  }

  // vai tentar resolver os tabuleiros gerados
  def tryBoards(boards: List[Board], blocks: Board): Option[Board] = {
      boards match { 
          // caso base: se não houver tabuleiros, retorna None
          case Nil => debugPrint("Nenhum tabuleiro válido encontrado"); None
          // caso recursivo: se houver tabuleiros, pega o primeiro e tenta resolver
          case b :: bs =>
              debugPrint(s"Tentando resolver tabuleiro: $b")
              solveBoard(b, blocks) match { // tenta resulver o primeiro da lista
                  case Some(solved) => debugPrint("Tabuleiro resolvido"); Some(solved) // se conseguir resolver, retorna o tabuleiro
                  case None => tryBoards(bs, blocks) // se não conseguir resolver, tenta o próximo tabuleiro
              }
      }
  }

  // seta o valor da célula no tabuleiro
  def updateBoard(board: Board, row: Int, column: Int, value: Option[Int]): Board = {
      board.updated(row, board(row).updated(column, value))
  }

  // verifica se todas as células estão definidas
  def allFilled(board: Board): Boolean = {
      debugPrint(s"Verificando se todas as células estão preenchidas: $board")
      board.flatten.forall(_.isDefined)
  }

  // verifica se o tabuleiro respeita as regras do jogo, fazendo um and entre todas as verificações
  def verify(board: Board, blocks: Board): Boolean = {
      val result = 
          noRepeatedNumberInBlock(board, blocks) &&
          noRepeatedNumberInAdjacentCells(board) &&
          noSmallerNumberOnTopOfBiggerNumber(board, blocks)
      debugPrint(s"Resultado da verificação: $result")
      result
  }

  // verifica se não há números repetidos em um bloco
  def uniqueElements(blocks: Board): List[List[(Int, Int)]] = {
    // transforma em uma única lista e pega os elementos distintos
    val uniqueValues = blocks.flatten.flatten.distinct 
    uniqueValues.map { value => // para cada valor, pega as posições no tabuleiro
      blocks.zipWithIndex.flatMap { case (blockRow, row) => // associa o índice da linha
        blockRow.zipWithIndex.collect { // coleta as posições em que a célul tem o valor atual
          case (cell, column) if cell.contains(value) => (row, column)
        }
      }
    }
  }

  // verifica se não há números repetidos em um bloco
  def noRepeatedNumberInBlock(board: Board, blocks: Board): Boolean = {
      // pega os blocos únicos ()
      val uniqueBlocks = uniqueElements(blocks)
      val result = uniqueBlocks.forall { block =>
          val values = block.flatMap { case (row, column) => board(row)(column) }
          values.distinct.length == values.length
      }
      debugPrint(s"Não há números repetidos: $result")
      result
  }

  def noRepeatedNumberInAdjacentCells(board: Board): Boolean = {
      val result = board.indices.forall { row =>
          board(row).indices.forall { column =>
              val cellValue = board(row)(column)
              val adjacentPositions = List((row-1, column), (row+1, column), (row, column-1), (row, column+1))
              val adjacentValues = adjacentPositions.collect {
                  case (row, column) if row >= 0 && row < board.length && column >= 0 && column < board(row).length => board(row)(column)
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