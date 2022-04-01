import org.scalatest._
import flatspec._
import matchers._
import Robot._

class RobotSpec extends AnyFlatSpec with should.Matchers {

  "advance()" should "return new position after moving a robot by 1 position according to current direction" in {
    advance((0, 0), 'n') shouldBe(0, 1)
    advance((5, 4), 'S') shouldBe(5, 3)
    advance((3, 3), 'w') shouldBe(2, 3)
    advance((3, 3), 'E') shouldBe(4, 3)
  }

  "turn()" should "return new direction after turning a robot according to given parameters" in {
    turn('W', 'R') shouldBe 'N'
    turn('W', 'R') should be('N')
    turn('s', 'l') shouldBe 'E'
    turn('S', 'R') shouldBe 'W'

  }

  "executeInstructions()" should "throw AssertionError if provided invalid input" in {
    a[AssertionError] should be thrownBy executeInstructions("INVALIDinstructions", (0, 0), 'N')
    a[AssertionError] should be thrownBy executeInstructions("AARL", (0, 0), 'X')
    a[AssertionError] should be thrownBy executeInstructions("INVALIDinstructions", (0, 0), 'X')
  }

  "executeInstructions()" should "return new position after executing instructions" in {
    executeInstructions("RAALAL", (7, 3), 'N') shouldBe(9, 4)
  }
}