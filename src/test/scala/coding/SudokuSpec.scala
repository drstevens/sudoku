package coding

import org.specs2.mutable.Specification
import scalaz._
import scalaz.Scalaz._

class SudokuSpec extends Specification {
  import Sudoku._

  "Sudo" should {

    "index lens should update the correct location" in {
      val empty = List.fill(3, 3)(None)
      val expected = List(
        List.fill(3)(None),
        List.fill(3)(None),
        List(None, Some(5), None)
      )

      val result = indexLens(1, 2).set(empty, 5.some)

      result ==== expected
    }

    "allow me to set the value at coordinates in a 2D arary" in {
      val input = List(
        Input(1, 0, 0),
        Input(2, 1, 0),
        Input(3, 3, 0),
        Input(6, 0, 1),
        Input(3, 2, 1),
        Input(7, 0, 2)
      )
      val board = initialize(input)

      val expected: List[List[Option[Int]]] = List(
        Some(1) ::Some(2) :: None :: Some(3) :: List.fill(5)(None),
        Some(6) :: None :: Some(3) :: List.fill(6)(None),
        Some(7) :: List.fill(8)(None),
        List.fill(9)(None),
        List.fill(9)(None),
        List.fill(9)(None),
        List.fill(9)(None),
        List.fill(9)(None),
        List.fill(9)(None)
      )

      board ==== expected
    }

    "shows the board" in {
      val input = List(
        Input(1, 0, 0),
        Input(2, 1, 0),
        Input(3, 3, 0)
      )

      val board = initialize(input)

      board.shows ====
        """
          |      0 1 2 3 4 5 6 7 8
          |      _ _ _ _ _ _ _ _ _
          |0 - | 1 2 x 3 x x x x x |
          |1 - | x x x x x x x x x |
          |2 - | x x x x x x x x x |
          |3 - | x x x x x x x x x |
          |4 - | x x x x x x x x x |
          |5 - | x x x x x x x x x |
          |6 - | x x x x x x x x x |
          |7 - | x x x x x x x x x |
          |8 - | x x x x x x x x x |""".stripMargin
    }


    "fill in a board which can be solved" in {
      val input: List[List[Option[Int]]] = List(
        n.map(_.some),
        List.fill(9)(None),
        List.fill(9)(None),
        List.fill(9)(None),
        List.fill(9)(None),
        List.fill(9)(None),
        List.fill(9)(None),
        List.fill(9)(None),
        List.fill(9)(None)
      )

      val result = solveBoard(input, 81)
      result.fold(
        errors => failure(errors.list.mkString),
        _ !=== None
      )
      println("Result: " + result.shows)
    }

    "fill in another board which can be solved" in {
      val input: List[List[Option[Int]]] = List(
        n.map(_.some),
        List.fill(9)(None),
        List.fill(9)(None),
        List.fill(9)(None),
        List.fill(9)(None),
        List.fill(9)(None),
        List.fill(9)(None),
        List.fill(9)(None),
        List(8, 7, 6, 5, 3, 4, 2, 1, 0).map(_.some)
      )

      val result = solveBoard(input, 81)
      result.fold(
        errors => failure(errors.list.mkString),
        _ !=== None
      )

      println("Result: " + result.shows)
    }

    "stop when an invalid board is detected" in {
      val input: List[List[Option[Int]]] = List(
        n.map(_.some),
        n.reverse.map(_.some),
        List.fill(9)(None),
        List.fill(9)(None),
        List.fill(9)(None),
        List.fill(9)(None),
        List.fill(9)(None),
        List.fill(9)(None),
        List.fill(9)(None)
      )

      val result = solveBoard(input, 81)
      result.fold(
        _.list ==== List(
          "Column %s has duplicate values %s".format(4, List(4, 4)),
          "Square %s has duplicate values %s".format(1, List(3, 4, 5, 5, 4, 3))
        ),
        s => failure("Should not have succeeded: %s".format(s.shows))
      )
    }
  }
}
