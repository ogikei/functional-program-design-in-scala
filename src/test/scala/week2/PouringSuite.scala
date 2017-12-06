package week2

import org.scalatest.FunSuite

class PouringSuite extends FunSuite {

  test("testing pouring algorithm") {
    val problem = new Pouring(Vector(4, 7))
    problem.moves
  }

}
