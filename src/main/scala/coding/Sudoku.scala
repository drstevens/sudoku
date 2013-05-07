package coding

import scalaz._
import Scalaz._
import Show._


object Sudoku {
  case class Input(value: Int, column: Int, row: Int)

  val emptyBoard = List.fill(9,9)(none[Int])

  //These lens's make it possible to "update" the value at a specific location in the game board.
  //They are really making it easy to return a new game board with the new values
  def listLens[T](i: Int): List[T] @> T =
    Lens(list => Store(value => list.updated(i, value), list(i)))
  def rowLens(i: Int):Lens[List[List[Option[Int]]], List[Option[Int]]] = listLens(i)
  def columnLens(i: Int): Lens[List[Option[Int]], Option[Int]] = listLens(i)
  def indexLens(column: Int, row: Int): Lens[List[List[Option[Int]]], Option[Int]] =  rowLens(row) andThen columnLens(column)

  /**
   * List from 0 < n < 9 because it is used in multiple places
   */
  val n = 0 until 9 toList
  val nAsSet = n toSet

  /**
   * Provides Show type class for the board type, required by usages of "shows"
   */
  implicit def showBoard = shows[List[List[Option[Int]]]] {
    board =>
      val rows = for {
        row <- n
      } yield (board(row).map {
          case None => "x"
          case Some(x) => x.shows
        }).mkString(row.shows + " - | ", " ", " |")
      rows.mkString(n.mkString("\n      ", " ", "\n      _ _ _ _ _ _ _ _ _\n"), "\n", "")
  }

  /**
   * Initialize a board from a list of input
   * @param input
   * @return
   */
  def initialize(input: List[Input]): List[List[Option[Int]]] =
    input.traverseS[List[List[Option[Int]]], Option[Int]] {
      case Input(value, column, row) => indexLens(column, row) := value.some
    } exec emptyBoard

  /**
   * Choose an location on the board, if it exists
   */
  def chooseEmptyIndex(board: List[List[Option[Int]]]): Option[(Int, Int)] = (for {
    row <- n.toStream
    column <- n.toStream
    if (indexLens(column, row).get(board).isEmpty)
  } yield (column, row)).headOption

  /**
   * Map (column, row) to square number
   */
  def squareNumber(column: Int, row: Int) = (row / 3 * 3) + (column / 3)


  case class MissingValues(column: List[Set[Int]], row: List[Set[Int]], square: List[Set[Int]])

  /**
   * This is used to validate the board and find the missing values of each type
   * @param board the board
   * @return Either a list of failures or the missing values
   */
  def findMissingValues(board: List[List[Option[Int]]]): ValidationNel[String, MissingValues] = {
    val columnValues: List[ValidationNel[String, Set[Int]]] = for {
      column <- n
      values = n.flatMap(row => indexLens(column, row).get(board))
      valuesAsSet = values.toSet
    } yield
      if (values.length /== valuesAsSet.size)
        "Column %s has duplicate values %s".format(column, values).failNel
      else
        (nAsSet &~ valuesAsSet).successNel

    val rowValues = for {
      row <- n
      values = n.flatMap(column => indexLens(column, row).get(board))
      valuesAsSet = values.toSet
    } yield
      if (values.length /== valuesAsSet.size)
        "Row %s has duplicate values %s".format(row, values).failNel
      else
        (nAsSet &~ valuesAsSet).successNel

    val squareValues = for {
      rowSq <- (0 until 3 toList)
      colSq <- (0 until 3 toList)
      values = for {
        rowLocal <- (0 until 3 toList)
        colLocal <- (0 until 3 toList)
        row = (rowSq * 3) + rowLocal
        col = (colSq * 3) + colLocal
        value <- indexLens(col, row).get(board)
      } yield value
      valuesAsSet = values.toSet
    } yield
      if (values.length /== valuesAsSet.size)
        "Square %s has duplicate values %s".format(rowSq * 3 + colSq, values).failNel
      else
        (nAsSet &~ valuesAsSet).successNel

    def sequenceVNel(values: List[ValidationNel[String, Set[Int]]]): ValidationNel[String, List[Set[Int]]] =
      values.sequence[({type l[a] = ValidationNel[String, a]})#l, Set[Int]]

    val combinedResult = sequenceVNel(columnValues) |@| sequenceVNel(rowValues) |@| sequenceVNel(squareValues)
    combinedResult(MissingValues)
  }

  def solveBoard(board: List[List[Option[Int]]], depth: Int): ValidationNel[String, Option[List[List[Option[Int]]]]] = {
    findMissingValues(board).flatMap {
      case MissingValues(missingColumnValues, missingRowValues, missingSquareValues) =>
        chooseEmptyIndex(board).fold(some(board).successNel[String]) {
          case (column, row) =>
            val square = squareNumber(column, row)
            val availableMissingValues = missingColumnValues(column).filter {
              v =>
                missingSquareValues(square).contains(v) && missingRowValues(row).contains(v)
            }
            availableMissingValues.foldLeft(none[List[List[Option[Int]]]].successNel[String]) {
              case (failure@Failure(_), _) => failure
              case (found@Success(Some(_)), _) => found
              case (Success(None), columnValue) =>
                solveBoard(indexLens(column, row).set(board, some(columnValue)), depth - 1)
            }
        }
    }
  }

}
