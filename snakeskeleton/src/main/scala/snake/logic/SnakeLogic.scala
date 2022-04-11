package snake.logic

import engine.random.{RandomGenerator, ScalaRandomGen}
import snake.game.{Apple, Direction, East, Empty, GridType, North, SnakeBody, SnakeHead, South, West}
import snake.logic.SnakeLogic._

/** To implement Snake, complete the ``TODOs`` below.
 *
 * If you need additional files,
 * please also put them in the ``snake`` package.
 */
class SnakeLogic(val randomGen: RandomGenerator,
                 val nrColumns: Int,
                 val nrRows: Int) {

  def this() = this(new ScalaRandomGen(), DefaultColumns, DefaultRows)

  var snake : Snake = new Snake(nrColumns, nrRows)
  var apple : (Int, Int) = (0, 0)
  var directionBuffer : List[Direction] = List()
  var reverseMode : Boolean = false

  initializeGame()

  def initializeGame() : Unit = {
    placeApple()
    snake.saveGameState(apple)
  }

  def setReverseTime(reverse: Boolean): Unit = reverseMode = reverse

  def isGameOver: Boolean = snake.bitItself

  def numberOfFreeCells(): (Int, Array[Array[Int]]) = {
    val freeCells = Array.ofDim[Int](nrRows, nrColumns)
    var index: Int = 0

    for (y <- 0 until nrRows ; x <- 0 until nrColumns) {
      if (getGridTypeAt(x,y) == Empty()) {
        freeCells(y)(x) = index
        index += 1
      }
      else freeCells(y)(x) = -1
    }
    (index, freeCells)
  }

  def setCoordinates(newApplePosition: Int, freeCellArray : Array[Array[Int]]): Unit = {
    for (y <- 0 until nrRows ; x <- 0 until nrColumns) {
      if (freeCellArray(y)(x) == newApplePosition) apple = (x,y)
    }
  }

  def placeApple(): Unit = {
    val (upTo, freeCells) = numberOfFreeCells()
    if (upTo == 0) apple = (-1, -1)
    else setCoordinates(randomGen.randomInt(upTo), freeCells)
  }

  def snakeAteApple(): Boolean = snake.getHeadPosition == apple

  def step(): Unit = {
    if (reverseMode){
      apple = snake.oldApplePosition()
      snake.moveBackwards()
    }
    else if (!isGameOver) {
      snake.saveGameState(apple)
      snake.move()
      if (snakeAteApple()) {
        placeApple()
        snake.growBy(grow = 3)
      }
      if(directionBuffer.nonEmpty) directionBuffer = directionBuffer.drop(directionBuffer.length)
    }
  }

  def notOppositeDirection(currentDir: Direction, previousDir: Direction): Boolean = {
    currentDir.opposite != previousDir
  }

  def changeDir(d: Direction): Unit = {

    if (directionBuffer.isEmpty) directionBuffer = directionBuffer :+ snake.getDirection
    directionBuffer = directionBuffer :+ d

    if (notOppositeDirection(d, snake.getDirection) && notOppositeDirection(d, directionBuffer.head)) {
      d match {
        case North() => snake.setDirection(d)
        case South() => snake.setDirection(d)
        case West() => snake.setDirection(d)
        case East() => snake.setDirection(d)
      }
    }
  }

  def isApple(x: Int, y: Int) :  Boolean = apple == (x,y)

  def getGridTypeAt(x: Int, y: Int): GridType = {
    if (snake.isHead(x, y)) SnakeHead(snake.getDirection)
    else if (snake.isBody(x, y)) SnakeBody(snake.color(x,y))
    else if (isApple(x,y)) Apple()
    else Empty()
  }
}

/** SnakeLogic companion object */
object SnakeLogic {

  val DefaultColumns = 10
  val DefaultRows = 10
}

class Snake(val nrColumns: Int, val nrRows: Int) {
  private var body: List[(Int,Int)] = List((0, 0), (1,0), (2,0)) //Last element is the head
  private var direction: Direction = East()
  private var needsToGrow: Int = 0
  private var gameState : List[List[(Int,Int)]] = List()

  def saveGameState(applePosition : (Int, Int)) : Unit ={
    var currentState: List[(Int,Int)] = List(applePosition)
    currentState = currentState :+ encodeDirection()
    currentState = currentState :+ (needsToGrow, 0)

    for (i <- body.indices){
      currentState = currentState :+ body(i)
    }
    gameState = gameState :+ currentState
  }

  def moveBackwards(): Unit = {
    if (gameState.length > 1) {
      direction = previousDirection()
      needsToGrow = gameState.last(2)._1

      body = body.drop(body.length)
      for (i <- 3 until gameState.last.length) {
        body = body :+ gameState.last(i)
      }
      gameState = gameState.dropRight(1)
    }
  }

  def move(): Unit = {
    moveAndWrapHead()
    if (needsToGrow > 0) needsToGrow -= 1
    else body = body.drop(1)
  }

  def moveAndWrapHead(): Unit = {
    val tempHead : Array[Int] = Array(body.last._1, body.last._2)
    direction match {
      case South() => tempHead(1) = (tempHead(1) + 1) % nrRows
      case North() => tempHead(1) = ((tempHead(1) - 1) % nrRows + nrRows) % nrRows
      case West() => tempHead(0) = ((tempHead(0) - 1) % nrColumns + nrColumns) % nrColumns
      case East() => tempHead(0) = (tempHead(0) + 1) % nrColumns
    }
    body = body :+ (tempHead(0), tempHead(1))
  }

  def encodeDirection(): (Int, Int) ={
    direction match {
      case North() => (1,0)
      case South() => (2,0)
      case East() => (3,0)
      case West() => (4,0)
    }
  }

  def previousDirection() : Direction ={
    gameState(gameState.length-2)(1)._1 match {
      case 1 => North()
      case 2 => South()
      case 3 => East()
      case 4 => West()
    }
  }

  def isBody(x: Int, y: Int): Boolean = {
    val cell : (Int,Int) = (x,y)
    body.contains(cell)
  }

  def bitItself: Boolean = {
    for (i <- 0 to body.length - 2){
      if (body(i) == body.last) return true
    }
    false
  }

  def oldApplePosition() : (Int, Int) = gameState.last.head

  def isHead(x: Int, y: Int): Boolean = body.last == (x,y)

  def growBy(grow: Int): Unit = needsToGrow += grow

  def setDirection(d: Direction): Unit = direction = d

  def getDirection: Direction = direction

  def getHeadPosition: (Int, Int) = body.last

  def color(x : Int, y : Int) : Float ={
    val cell : (Int, Int) = (x,y)
    1 - (body.indexOf(cell) / body.length.toFloat)
  }
}