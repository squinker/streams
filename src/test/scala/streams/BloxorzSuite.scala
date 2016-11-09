package streams

import org.scalatest.FunSuite
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import Bloxorz._

@RunWith(classOf[JUnitRunner])
class BloxorzSuite extends FunSuite {

  trait SolutionChecker extends GameDef with Solver with StringParserTerrain {
    /**
     * This method applies a list of moves `ls` to the block at position
     * `startPos`. This can be used to verify if a certain list of moves
     * is a valid solution, i.e. leads to the goal.
     */
    def solve(ls: List[Move]): Block =
      ls.foldLeft(startBlock) { case (block, move) => move match {
        case Left => block.left
        case Right => block.right
        case Up => block.up
        case Down => block.down
      }
    }
  }

  trait tester extends GameDef {


  }

  trait Level1 extends SolutionChecker {
      /* terrain for level 1*/

    val level =
    """ooo-------
      |oSoooo----
      |ooooooooo-
      |-ooooooooo
      |-----ooToo
      |------ooo-""".stripMargin

    val optsolution = List(Right, Right, Down, Right, Right, Right, Down)
  }


  /*
	test("terrain function level 1") {
    new Level1 {
      assert(terrain(Pos(0,0)), "0,0")
      assert(terrain(Pos(1,1)), "1,1") // start
      assert(terrain(Pos(4,7)), "4,7") // goal
      assert(terrain(Pos(5,8)), "5,8")
      assert(!terrain(Pos(5,9)), "5,9")
      assert(terrain(Pos(4,9)), "4,9")
      assert(!terrain(Pos(6,8)), "6,8")
      assert(!terrain(Pos(4,11)), "4,11")
      assert(!terrain(Pos(-1,0)), "-1,0")
      assert(!terrain(Pos(0,-1)), "0,-1")
    }
  }

	test("findChar level 1") {
    new Level1 {
      assert(startPos == Pos(1,1))
    }
  }

  new Level1{
    test("whether standing block is standing") {
      val standingBlock = Block(Pos(0, 1), Pos(0, 1))
      assert( standingBlock.isStanding )
    }
  }

  new Level1{
    test("whether non-standing block is non-standing") {
      val notStandingBlock = Block(Pos(0, 1), Pos(0, 2))
      assert( ! notStandingBlock.isStanding )

    }
  }



  new Level1{
    test("whether block inside terrain is legal") {
      val blockInsideTerrain = Block(Pos(0, 0), Pos(0, 0))
      assert( blockInsideTerrain.isLegal )
    }
  }

  new Level1{
    test("whether block half inside terrain is illegal") {
      val blockInsideTerrain = Block(Pos(2, 0), Pos(3, 0))
      assert(! blockInsideTerrain.isLegal )
    }
  }

  new Level1{
    test("whether block totally outside terrain is illegal") {
      val blockInsideTerrain = Block(Pos(3, 0), Pos(4, 0))
      assert(! blockInsideTerrain.isLegal )
    }
  }

*/


  /*

  Never terminates
  new Level1{
    test("Test from") {

      val stream = (Block(Pos(1, 1), Pos(1, 1)), List(Left, Up)) #:: Stream.Empty

      val res = from(stream,Set())

      println("** From result is" + res.toSet)
      assert(1==1)
    }
  }
  */




	test("optimal solution for level 1") {
    new Level1 {
      assert(solve(solution) == Block(goal, goal))
    }
  }



	test("optimal solution length for level 1") {
    new Level1 {
      assert(solution.length == optsolution.length)
    }
  }

  /*

  test("neighbours for block starting at 2,2 2,2") {


    new Level1 {

      val s = Block(Pos(2,2), Pos(2,2))

      val leftBlock  = Block(Pos(2,0), Pos(2,1))
      val upBlock    = Block(Pos(0,2), Pos(1,2))
      val rightBlock = Block(Pos(2,3), Pos(2,4))
      val downBlock  = Block(Pos(3,2), Pos(4,2))

        assert( s.neighbors == List((leftBlock, Left), (upBlock, Up), (rightBlock, Right), (downBlock, Down)) )
    }
  }

  test("neighbours for block starting at 0,0 0,0") {


    new Level1 {

      val s = Block(Pos(0,0), Pos(0,0))

      val leftBlock  = Block(Pos(0,-2), Pos(0,-1))
      val upBlock    = Block(Pos(-2,0), Pos(-1,0))
      val rightBlock = Block(Pos(0,1), Pos(0,2))
      val downBlock  = Block(Pos(1,0), Pos(2,0))

      assert( s.neighbors == List((leftBlock, Left), (upBlock, Up), (rightBlock, Right), (downBlock, Down)) )
    }
  }

  test("legal neighbours should not include those squares outside the terrain") {


    new Level1 {

      val s = Block(Pos(0,0), Pos(0,0))

      val rightBlock = Block(Pos(0,1), Pos(0,2))
      val downBlock  = Block(Pos(1,0), Pos(2,0))

      assert( s.legalNeighbors == List((rightBlock, Right), (downBlock, Down)) )
    }
  }

  test("legal neighbours should not include illegal positions") {

    new Level1 {

      val s = Block(Pos(2,2), Pos(2,2))

      val leftBlock  = Block(Pos(2,0), Pos(2,1))
      val upBlock    = Block(Pos(0,2), Pos(1,2))
      val rightBlock = Block(Pos(2,3), Pos(2,4))


      assert( s.legalNeighbors == List((leftBlock, Left), (upBlock, Up), (rightBlock, Right)) )
    }

  }


  test("neigbours with history from instructions") {

    new Level1 {

      val s = Block(Pos(2,2), Pos(2,2))

      val leftBlock  = Block(Pos(2,0), Pos(2,1))
      val stream = neighborsWithHistory(Block(Pos(1,1),Pos(1,1)), List(Left,Up,Up)) toSet

      assert(stream == Set(
        (Block(Pos(1,2),Pos(1,3)), List(Right,Left,Up, Up)),
        (Block(Pos(2,1),Pos(3,1)), List(Down,Left,Up, Up))
      )
      )
    }
  }




  test("neigbours with history from instructions only take 1 ") {

    new Level1 {

      val s = Block(Pos(2,2), Pos(2,2))

      val leftBlock  = Block(Pos(2,0), Pos(2,1))
      val stream = neighborsWithHistory(Block(Pos(1,1),Pos(1,1)), List(Left,Up)) take 1 toSet

      assert(stream == Set(
        (Block(Pos(1,2),Pos(1,3)), List(Right,Left,Up))
      )
      )
    }
  }


  test("new neighbours only removes circles") {
    new Level1 {

      val s = Block(Pos(2, 2), Pos(2, 2))

      val neighbors = Set(
        (Block(Pos(1,2),Pos(1,3)), List(Right,Left,Up)),
        (Block(Pos(2,1),Pos(3,1)), List(Down,Left,Up))
      ).toStream

      val explored =  Set(
        Block(Pos(1,2),Pos(1,3)),
        Block(Pos(1,1),Pos(1,1)))

      val expected = Set(
        (Block(Pos(2,1),Pos(3,1)), List(Down,Left,Up))
      ).toStream

      assert(
        newNeighborsOnly(
          neighbors = neighbors,
          explored = explored
        ) == expected)
    }
  }

  test("Test done") {

    new Level1 {

      assert(   done(Block(Pos(4,7), Pos(4,7))) )
      //assert( ! done(Block(Pos(4,7), Pos(4,8))) )
      //assert( ! done(Block(Pos(0,0), Pos(0,0))) )
    }


  }
*/
}
