/**
 *
 */
import scala.util.Random
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.Map

/**
 * @author kalman
 *
 */
object TicTacToe extends Population {
  
  override def generateRandomGenome(index: Int, random: Random) = {
    random.nextInt(reducedMoves(index).length)
  }

  override def pCrossOver = 0.15

  def isWin(arr: Array[Char], ch: Char) = {
    (arr(0) == ch && arr(1) == ch && arr(2) == ch) ||
      (arr(3) == ch && arr(4) == ch && arr(5) == ch) ||
      (arr(6) == ch && arr(7) == ch && arr(8) == ch) ||
      (arr(0) == ch && arr(3) == ch && arr(6) == ch) ||
      (arr(1) == ch && arr(4) == ch && arr(7) == ch) ||
      (arr(2) == ch && arr(5) == ch && arr(8) == ch) ||
      (arr(0) == ch && arr(4) == ch && arr(8) == ch) ||
      (arr(2) == ch && arr(4) == ch && arr(6) == ch)
  }

  def table2String(table: BigInt) = ("000000000" + table.toString(3)).takeRight(9)

  def string2Table(tableStr: String) = BigInt(tableStr, 3)

  def move(strTable: String, moveChar: Char, index: Int) = {
    var pos = index
    var charArray = strTable.toCharArray
    for (i <- 0 until charArray.length) {
      if (pos >= 0) {
        if (charArray(i) == '0') {
          if (pos == 0) charArray(i) = moveChar
          pos -= 1
        }
      }
    }
    String.valueOf(charArray)
  }

  def reducedMove(strTable: String, moveChar: Char, index: Int) = {
    val charArray = strTable.toCharArray
    charArray(index) = moveChar
    String.valueOf(charArray)
  }
  
  def showTable(table: Int) = {
    val strArr = table2String(table)
    val strTable = strArr.replace('0', ' ').replace('1', 'O').replace('2', 'X')
    println(strTable.substring(0, 3))
    println(strTable.substring(3, 6))
    println(strTable.substring(6, 9))
    println("___")
  }

  def moveToPos(tableIndex: BigInt, step: Int, pos: Int, show: Boolean): (BigInt, (Int, Int, Int)) = {
    val moveChar = ("" + (step % 2 + 1))(0)

    var table = tableIndex

    var transformIndex = 0

    if (mapTableToReduced.contains(table.intValue)) {
      val reduced = mapTableToReduced(table.intValue)
      transformIndex = reducedTables(reduced).indexOf(table.intValue)
      table = (getReduced(reducedTables(reduced)))
    }
    val strTable = table2String(table)

    val strArr = move(strTable, moveChar, pos)
    table = string2Table(transform(strArr, transformIndex))

    if (isWin(strArr.toCharArray(), moveChar)) {
      if (step % 2 == 0) (table, (0, 0, 1)) else (table, (1, 0, 0))
    } else (table, (0, 1, 0))
  }

  def moveChromosoma(tableIndex: BigInt, step: Int, chrom: Chromosoma, show: Boolean): (BigInt, (Int, Int, Int)) = {
    val moveChar = ("" + (step % 2 + 1))(0)

    var table = tableIndex

    var transformIndex = 0
    var pos = 0

    if (mapTableToReduced.contains(table.intValue)) {
      val reduced = mapTableToReduced(table.intValue)
      transformIndex = reducedTables(reduced).indexOf(table.intValue)
      pos = chrom.genome(reduced)
      table = (getReduced(reducedTables(reduced)))
      pos = reducedMoves(reduced)(pos)
    }
    val strTable = table2String(table)

    val strArr = move(strTable, moveChar, pos)
    table = string2Table(transform(strArr, transformIndex))

    if (isWin(strArr.toCharArray(), moveChar)) {
      if (step % 2 == 0) (table, (0, 0, 1)) else (table, (1, 0, 0))
    } else (table, (0, 1, 0))
  }

  def evaluate(chromosoma: Chromosoma) = {

    def loop(table: BigInt, step: Int, player: Int, move: Int, sum: (Int, Int, Int)): (Int, Int, Int) = {
      if (step > 9) {
        (sum._1, sum._2 + 1, sum._3)
      } else {
        val (tableIndex, (win, draw, lost)) = if (move >= 0) {
	      val (tableIndex, (win, draw, lost)) = moveToPos(table, step, move, false)
	      
	      if (player == 1) (tableIndex, (win, draw, lost)) 
	      else (tableIndex, (lost, draw, win)) 
        } else if (step % 2 == player) {
          val (tableIndex, (win, draw, lost)) = moveChromosoma(table, step, chromosoma, false)
	      
          if (player == 1) (tableIndex, (win, draw, lost)) 
	      else (tableIndex, (lost, draw, win)) 
        } else {
          if (mapTableToReduced.contains(table.intValue)) {
            val reducedIndex = mapTableToReduced(table.intValue)
            val reduced = getReduced(reducedTables(reducedIndex))
            val moves = reducedMoves(reducedIndex)
            (null, moves.foldLeft( (0, 0, 0) )((point, pos) => loop(reduced, step, player, pos, point)))
          } else {
            (null, loop(table, step, player, 0, (0, 0, 0)))
          }
        }

        if (tableIndex == null || draw == 0) {
          (sum._1 + win, sum._2 + draw, sum._3 + lost)
        } else {
          loop(tableIndex, step + 1, player, -1, sum)
        }
      }
    }
    
    val result1 = loop(BigInt(0), 1, 1, -1, (0, 0, 0))
    val result2 = loop(BigInt(0), 1, 0, -1, (0, 0, 0))
    chromosoma.fitness = ((result1._1 + result2._1) * 100.0 + (result1._2 + result2._2) * 100.0 + (result1._3 + result2._3) * 0) /
    	(result1._1 + result2._1 + result1._2 + result2._2 + result1._3 + result2._3)
    
    chromosoma.user = (result1._1 + result2._1, result1._2 + result2._2, result1._3 + result2._3)
    
    chromosoma
  }

  override def exit = chromosomas.head.user match {
    case (win, draw, lost) => lost == 0
    case _ => false
  }
    
  override def show(chromosoma: Chromosoma) = {
    println(step + ". " + chromosoma.fitness + ", " + chromosomas.last.fitness + " [" + chromosoma.user + "]")
  }

  val mapTableToReduced = Map[Int, Int]()

  val reducedTables = ArrayBuffer[List[Int]]()

  val transforms = List("012345678", "258147036", "876543210", "630741852",
    "036147258", "678345012", "852741630", "210543876")

  val reducedEmptyCounts = ArrayBuffer[Int]()

  val reducedMoves = ArrayBuffer[Seq[Int]]()

  def transform(strTable: String, transformIndex: Int) = {
    val transform = transforms(transformIndex)
    transform.map(ch => strTable(ch - '0'))
  }

  private def countChar(str: String, char: Char) = str.foldLeft(0) {
    (sum, ch) => if (char == ch) sum + 1 else sum
  }

  private def getReduced(transformedTables: Seq[Int]) = {
    transformedTables.foldLeft(20000)((min, reduced) => if (reduced < min) reduced else min)
  }

  def init = {
    for (i <- 0 until 19683) {
      val strTable = table2String(BigInt(i))
      val transformedTables = (0 until transforms.length).map(tr => string2Table(transform(strTable, tr)).intValue)
      val reduced = getReduced(transformedTables)
      if (reduced == i) {
        val emptyCounts = countChar(strTable, '0')
        val oCounts = countChar(strTable, '1')
        val xCounts = countChar(strTable, '2')

        val charArray = strTable.toCharArray
        val isWinO = isWin(charArray, '1')
        val isWinX = isWin(charArray, '2')

        if (emptyCounts > 1 && !isWinO && !isWinX && (xCounts == oCounts || xCounts == oCounts + 1)) {
          reducedEmptyCounts.append(emptyCounts)

          reducedTables.append(transformedTables.toList)
          val reducedIndex = reducedTables.length - 1

          transformedTables.map(table => {
            if (!mapTableToReduced.contains(table)) {
              mapTableToReduced.put(table, reducedIndex)
            }
          })
        }
      }
    }

    (0 until reducedTables.length).map(reducedIndex => {
      val reduced = getReduced(reducedTables(reducedIndex))
      val strTable = table2String(BigInt(reduced))

      val emptyCounts = countChar(strTable, '0')
      val oCounts = countChar(strTable, '1')
      val xCounts = countChar(strTable, '2')

      val moveChar = if (xCounts == oCounts) '2' else '1'

      val legalMoves = (0 until emptyCounts).map(index => {
        val movedTable = move(strTable, moveChar, index)
        val movedReducedTable = string2Table(movedTable).intValue

        if (mapTableToReduced.contains(movedReducedTable)) {
          mapTableToReduced(movedReducedTable) -> index
        } else {
          (1000 + index) -> index
        }
      }).toMap
      reducedMoves.append(legalMoves.toList.map(entry => entry._2))
    })
  }

  init
}
