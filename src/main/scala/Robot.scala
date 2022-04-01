object Robot {

  // Seq of directions in clock-wise order for easy turning
  val directions = Seq('N', 'E', 'S', 'W')


  def advance(coordinates: (Int, Int), direction: Char): (Int, Int) = {
    val (x, y) = coordinates
    direction.toUpper match {
      case 'N' => (x, y + 1)
      case 'S' => (x, y - 1)
      case 'E' => (x + 1, y)
      case 'W' => (x - 1, y)
    }
  }


  def turn(direction: Char, side: Char): Char = side.toUpper match {
    case 'R' =>
      val oldIndex = directions.indexOf(direction.toUpper)
      val newIndex = (oldIndex + 1) % 4
      directions(newIndex)
    case 'L' =>
      val oldIndex = directions.indexOf(direction.toUpper)
      val newIndex = if (oldIndex == 0) 3 else oldIndex - 1
      directions(newIndex)
  }


  def isInputValid(instructions: String, startDir: Char) = {
    val validInstructions: Boolean = instructions.forall(Seq('A', 'R', 'L').contains)
    val validStartDir: Boolean = directions.contains(startDir)

    validInstructions && validStartDir
  }

  def executeInstructions(instructions: String, startPos: (Int, Int), startDir: Char): (Int, Int) = {
    assert(isInputValid(instructions, startDir))

    var direction = startDir
    instructions.foldLeft(startPos) { (pos, instruction) =>
      instruction match {
        case 'A' => advance(pos, direction)
        case 'R' | 'L' =>
          direction = turn(direction, instruction)
          pos
      }
    }
  }
}
