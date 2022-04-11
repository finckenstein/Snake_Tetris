package tetris.logic

import engine.random.{RandomGenerator, ScalaRandomGen}
import tetris.game._
import tetris.logic.TetrisLogic._

/** To implement Tetris, complete the ``TODOs`` below.
  *
  * If you need additional files,
  * please also put them in the ``tetris`` package.
  */
class TetrisLogic(val randomGen: RandomGenerator,
                  val nrColumns: Int,
                  val nrRows: Int,
                  val initialBoard: Seq[Seq[TetrisBlock]]) {

  def this(random: RandomGenerator, nrColumns: Int, nrRows: Int) =
    this(random, nrColumns, nrRows, makeEmptyBoard(nrColumns, nrRows))

  def this() =
    this(new ScalaRandomGen(), DefaultWidth, DefaultHeight, makeEmptyBoard(DefaultWidth, DefaultHeight))

  var tetrisBlock : Tetrominos = new Tetrominos (nrColumns, nrRows, randomGen, initialBoard)
  val middle : Int = nrColumns / 2
  tetrisBlock.initializeGame(middle)

  def rotateLeft(): Unit = {
    if (!tetrisBlock.rotationBlockedByBlocks(right = false) && !isGameOver){
      tetrisBlock.setRotationIndex(valueCheck = 0, value = 3, incrementOrDecrement = -1)
      tetrisBlock.preventOutOfBoundsRotation()
    }
  }

  def rotateRight(): Unit = {
    if (!tetrisBlock.rotationBlockedByBlocks(right = true) && !isGameOver) {
      tetrisBlock.setRotationIndex(valueCheck = 3, value = 0, incrementOrDecrement = 1)
      tetrisBlock.preventOutOfBoundsRotation()
    }
  }

  def moveLeft(): Unit = {
    if(!tetrisBlock.reachedHorizontalEnd(leftOrRightEnd = 0) && !tetrisBlock.movementBlockedByBlocks(xTranslation = -1) && !isGameOver){
      tetrisBlock.xTransition(xTranslation = -1)
    }
  }

  def moveRight(): Unit = {
    if(!tetrisBlock.reachedHorizontalEnd(leftOrRightEnd = nrColumns - 1) && !tetrisBlock.movementBlockedByBlocks(xTranslation = 1) && !isGameOver){
      tetrisBlock.xTransition(xTranslation = 1)
    }
  }
  
  def addBlockToBlockPileAndDeleteLines(): Unit ={
    tetrisBlock.addToBlockPile()
    tetrisBlock.deleteLinesInBlockPile()
  }

  def moveDown(): Unit = {
    if (!isGameOver) {
      if (tetrisBlock.reachedVerticalEnd() || tetrisBlock.touchedOtherBlocks()) {
        addBlockToBlockPileAndDeleteLines()
        tetrisBlock.generateNewBlock(middle)
      }
      else tetrisBlock.updateVerticalPosition(middle, initializeBlock = false)
    }
  }

  def doHardDrop(): Unit = {
    if(!isGameOver) {
      while (!tetrisBlock.reachedVerticalEnd() && !tetrisBlock.touchedOtherBlocks()) {
        tetrisBlock.updateVerticalPosition(middle, initializeBlock = false)
      }
      addBlockToBlockPileAndDeleteLines()
      tetrisBlock.generateNewBlock(middle)
    }
  }
  def isGameOver: Boolean = tetrisBlock.isSpawnPlaceOccupied(middle)
  def getBlockAt(x: Int, y: Int): TetrisBlock = tetrisBlock.drawBoard(x,y, middle)
}

object TetrisLogic {

  def makeEmptyBoard(nrColumns: Int, nrRows: Int): Seq[Seq[TetrisBlock]] = {
    val emptyLine = Seq.fill(nrColumns)(Empty)
    Seq.fill(nrRows)(emptyLine)
  }

  val DefaultWidth: Int = 10
  val NrTopInvisibleLines: Int = 4
  val DefaultVisibleHeight: Int = 20
  val DefaultHeight: Int = DefaultVisibleHeight + NrTopInvisibleLines


  def apply() = new TetrisLogic(new ScalaRandomGen(),
    DefaultWidth,
    DefaultHeight,
    makeEmptyBoard(DefaultWidth, DefaultHeight))
}

class Tetrominos (val nrColumns: Int, val nrRows: Int, val randomGen: RandomGenerator, val initialBoard: Seq[Seq[TetrisBlock]]){
  private var currentTetromino : Array[Array[(Int, Int)]] = Array(Array((0,0)))
  private var blockType : TetrisBlock = Empty
  private var blockInteger : Int = randomGen.randomInt(upTo = 7)
  private var indexToRotatedBlock : Int = 0
  private val blockPile = Array.ofDim[TetrisBlock](nrColumns, nrRows)

  def isEven(): Boolean = if (nrColumns % 2 == 0) true else false

  def initializeGame(middle : Int): Unit ={
    updateVerticalPosition(middle, initializeBlock = true)
    for (i <- 0 until nrRows; j <- 0 until nrColumns) blockPile(j)(i) = initialBoard(i)(j)
  }

  //functions to handle left & right rotations
  def rotationBlockedByBlocks(right : Boolean): Boolean ={
    val tempRotation : Int = indexToRotatedBlock
    val tempBlock : Array[(Int, Int)] = currentTetromino(indexToRotatedBlock)
    if (right) setRotationIndex(valueCheck = 3, value = 0, incrementOrDecrement = 1)
    else setRotationIndex(valueCheck = 0, value = 3, incrementOrDecrement = -1)
    preventOutOfBoundsRotation()

    for (i <- 0 until 4){
      if (blockPile(currentTetromino(indexToRotatedBlock)(i)._1)(currentTetromino(indexToRotatedBlock)(i)._2) != Empty){
        toPreviousBlock(tempRotation, tempBlock)
        return true
      }
    }
    toPreviousBlock(tempRotation, tempBlock)
    false
  }

  def toPreviousBlock(tempRotation : Int, tempBlock : Array[(Int, Int)]): Unit ={
    indexToRotatedBlock = tempRotation
    currentTetromino(indexToRotatedBlock) = tempBlock
  }

  def preventOutOfBoundsRotation() : Unit ={
    for (i <- 0 until 4){
      if (currentTetromino(indexToRotatedBlock)(i)._1 >= nrColumns) xTransition(xTranslation = -1)
      else if (currentTetromino(indexToRotatedBlock)(i)._1 <= -1) {
        xTransition(xTranslation = 1)
        if (blockType == IBlock && indexToRotatedBlock == 2) xTransition(xTranslation = 1)
      }
    }
  }

  def setRotationIndex(valueCheck : Int, value : Int, incrementOrDecrement : Int): Unit ={
    if (indexToRotatedBlock == valueCheck) indexToRotatedBlock = value
    else indexToRotatedBlock = indexToRotatedBlock + incrementOrDecrement
  }

  //functions to handle left & right movement
  def reachedHorizontalEnd(leftOrRightEnd : Int) : Boolean ={
    for (i <- currentTetromino.indices){
      if (currentTetromino(indexToRotatedBlock)(i)._1 == leftOrRightEnd) return true
    }
    false
  }
  def movementBlockedByBlocks(xTranslation : Int): Boolean = {
    for (i <- 0 until 4){
      if (blockPile(currentTetromino(indexToRotatedBlock)(i)._1 + xTranslation)(currentTetromino(indexToRotatedBlock)(i)._2) != Empty) return true
    }
    false
  }
  def xTransition(xTranslation : Int): Unit = {
    for (i <- 0 until 4; j <- 0 until 4) currentTetromino(i)(j) = (currentTetromino(i)(j)._1 + xTranslation, currentTetromino(i)(j)._2)
  }

  //functions that check what the block touched
  def reachedVerticalEnd() : Boolean ={
    for (i <- currentTetromino.indices){
      if (currentTetromino(indexToRotatedBlock)(i)._2 == nrRows - 1) return true
    }
    false
  }
  def touchedOtherBlocks(): Boolean ={
    for (i <- 0 until 4){
      if (blockPile(currentTetromino(indexToRotatedBlock)(i)._1)(currentTetromino(indexToRotatedBlock)(i)._2 + 1) != Empty) return true
    }
    false
  }

  def generateNewBlock (middle : Int): Unit ={
    blockInteger = randomGen.randomInt(upTo = 7)
    indexToRotatedBlock = 0
    updateVerticalPosition(middle, initializeBlock = true)
  }
  
  //adding & removing to blockPile
  def removeRow(row : Int) : Unit ={
    val tempBlockPile = Array.ofDim[TetrisBlock](nrColumns, nrRows)
    for (y <- 0 until row; x <- 0 until nrColumns){
      tempBlockPile(x)(y) = blockPile(x)(y)
    }
    for (y <- 1 to row; x <- 0 until nrColumns){
      blockPile(x)(y) = tempBlockPile(x)(y-1)
    }
  }
  def deleteLinesInBlockPile(): Unit ={
    var rowContainsEmptyCell : Boolean = false
    for (y <- 0 until nrRows){
      for (x <- 0 until nrColumns){
        if (blockPile(x)(y) == Empty) rowContainsEmptyCell = true
      }
      if (!rowContainsEmptyCell) removeRow(y)
      rowContainsEmptyCell = false
    }
  }
  def addToBlockPile(): Unit ={
    for (i <- 0 until 4){
      blockPile(currentTetromino(indexToRotatedBlock)(i)._1)(currentTetromino(indexToRotatedBlock)(i)._2) = blockType
    }
  }

  //GameOver and drawing Board functions
  def blockIsAtInitialPosition(): Boolean ={
    var counter : Int = 0
    for (x <- 0 until nrColumns; y <- 0 until 2; i <- 0 until 4){
      if (currentTetromino(indexToRotatedBlock)(i) == (x,y)) counter += 1
    }
    if (counter == 4) true
    else false
  }
  def isSpawnPlaceOccupied(middle : Int): Boolean ={
    if (blockIsAtInitialPosition()){
      for (i <- 0 until 4){
        if (blockPile(currentTetromino(0)(i)._1)(currentTetromino(0)(i)._2) != Empty) return true
      }
    }
    false
  }
  def drawBoard(x: Int, y: Int, middle : Int): TetrisBlock ={
    for (i <- 0 until 4) {
      if (currentTetromino(indexToRotatedBlock)(i)._1 == x && currentTetromino(indexToRotatedBlock)(i)._2 == y)return blockType
    }
    blockPile(x)(y)
  }

  //block's vertical movement
  def updateVerticalPosition(middle : Int, initializeBlock : Boolean) : Unit ={
    blockInteger match {
      case 0 => if (initializeBlock) initializeIBlock(middle) else yTranslation()
      case 1 => if (initializeBlock) initializeJBlock(middle) else yTranslation()
      case 2 => if (initializeBlock) initializeLBlock(middle) else yTranslation()
      case 3 => if (initializeBlock) initializeOBlock(middle) else yTranslation()
      case 4 => if (initializeBlock) initializeSBlock(middle) else yTranslation()
      case 5 => if (initializeBlock) initializeTBlock(middle) else yTranslation()
      case 6 => if (initializeBlock) initializeZBlock(middle) else yTranslation()
    }
  }

  def yTranslation() : Unit = {
    for (i <- 0 until 4 ; j <- 0 until 4) currentTetromino(i)(j) = (currentTetromino(i)(j)._1, currentTetromino(i)(j)._2 + 1)
  }
  def initializeIBlock(middle : Int): Unit ={
    if (isEven()) currentTetromino = Array (Array((middle - 2, 1), (middle - 1, 1), (middle, 1), (middle + 1, 1)),
        Array((middle, 0), (middle, 1), (middle, 2), (middle, 3)),
        Array((middle - 2, 2), (middle - 1, 2), (middle, 2), (middle + 1, 2)),
        Array((middle - 1, 0), (middle - 1, 1), (middle - 1, 2), (middle - 1, 3)))
    else currentTetromino = Array (Array((middle - 1, 1), (middle, 1), (middle + 1, 1), (middle + 2, 1)),
        Array((middle + 1, 0), (middle + 1, 1), (middle + 1, 2), (middle + 1, 3)),
        Array((middle - 1, 2), (middle, 2), (middle + 1, 2), (middle + 2, 2)),
        Array((middle, 0), (middle, 1), (middle, 2), (middle, 3)))
    blockType = IBlock
  }
  def initializeJBlock(middle : Int): Unit ={
    if (isEven())currentTetromino = Array(Array((middle - 2, 0), (middle - 2, 1), (middle - 1, 1), (middle, 1)),
        Array((middle, 0), (middle - 1, 0), (middle - 1, 1), (middle - 1, 2)),
        Array((middle, 2), (middle, 1), (middle - 1, 1), (middle - 2, 1)),
        Array((middle - 2, 2), (middle - 1, 2), (middle - 1, 1), (middle - 1, 0)))
    else currentTetromino = Array(Array((middle - 1, 0), (middle - 1, 1), (middle, 1), (middle + 1, 1)),
        Array((middle + 1, 0), (middle, 0), (middle, 1), (middle, 2)),
        Array((middle + 1, 2), (middle + 1, 1), (middle, 1), (middle - 1, 1)),
        Array((middle - 1, 2), (middle, 2), (middle, 1), (middle, 0)))
    blockType = JBlock
  }
  def initializeLBlock(middle : Int): Unit ={
    if (isEven()) currentTetromino = Array (Array((middle - 2, 1), (middle - 1, 1), (middle, 1), (middle, 0)),
        Array((middle - 1, 0), (middle - 1, 1), (middle - 1, 2), (middle, 2)),
        Array((middle, 1), (middle - 1, 1), (middle - 2, 1), (middle - 2, 2)),
        Array((middle - 1, 2), (middle - 1, 1), (middle - 1, 0), (middle - 2, 0)))
    else currentTetromino = Array(Array((middle - 1, 1), (middle, 1), (middle + 1, 1), (middle + 1, 0)),
        Array((middle, 0), (middle, 1), (middle, 2), (middle + 1, 2)),
        Array((middle + 1, 1), (middle, 1), (middle - 1, 1), (middle - 1, 2)),
        Array((middle, 2), (middle, 1), (middle, 0), (middle - 1, 0)))
    blockType = LBlock
  }
  def initializeOBlock(middle : Int): Unit ={
    if (isEven()) currentTetromino = Array(Array((middle - 1, 0), (middle, 0), (middle - 1, 1), (middle, 1)),
        Array((middle - 1, 0), (middle, 0), (middle - 1, 1), (middle, 1)),
        Array((middle - 1, 0), (middle, 0), (middle - 1, 1), (middle, 1)),
        Array((middle - 1, 0), (middle, 0), (middle - 1, 1), (middle, 1)))
    else currentTetromino = Array(Array((middle, 0), (middle + 1, 0), (middle, 1), (middle + 1, 1)),
        Array((middle, 0), (middle + 1, 0), (middle, 1), (middle + 1, 1)),
        Array((middle, 0), (middle + 1, 0), (middle, 1), (middle + 1, 1)),
        Array((middle, 0), (middle + 1, 0), (middle, 1), (middle + 1, 1)))
    blockType = OBlock
  }
  def initializeSBlock(middle : Int): Unit ={
    if (isEven()) currentTetromino = Array(Array((middle - 2, 1), (middle - 1, 1), (middle - 1, 0), (middle, 0)),
        Array((middle - 1, 0), (middle - 1, 1), (middle, 1), (middle, 2)),
        Array((middle, 1), (middle - 1, 1), (middle - 1, 2), (middle - 2, 2)),
        Array((middle - 1, 2), (middle - 1, 1), (middle - 2, 1), (middle - 2, 0)))
    else currentTetromino = Array(Array((middle - 1, 1), (middle, 1), (middle, 0), (middle + 1, 0)),
        Array((middle, 0), (middle, 1), (middle + 1, 1), (middle + 1, 2)),
        Array((middle + 1, 1), (middle, 1), (middle, 2), (middle - 1, 2)),
        Array((middle, 2), (middle, 1), (middle - 1, 1), (middle - 1, 0)))
    blockType = SBlock
  }
  def initializeTBlock(middle : Int): Unit ={
    if (isEven()) currentTetromino = Array(Array((middle - 2, 1), (middle - 1, 1), (middle - 1, 0), (middle, 1)),
        Array((middle - 1, 0), (middle - 1, 1), (middle, 1), (middle - 1, 2)),
        Array((middle, 1), (middle - 1, 1), (middle - 1, 2), (middle - 2, 1)),
        Array((middle - 1, 2), (middle - 1, 1), (middle - 2, 1), (middle - 1, 0)))
    else currentTetromino = Array(Array((middle - 1, 1), (middle, 1), (middle, 0), (middle + 1, 1)),
        Array((middle, 0), (middle, 1), (middle + 1, 1), (middle, 2)),
        Array((middle + 1, 1), (middle, 1), (middle, 2), (middle - 1, 1)),
        Array((middle, 2), (middle, 1), (middle - 1, 1), (middle, 0)))
    blockType = TBlock
  }
  def initializeZBlock(middle : Int): Unit ={
    if (isEven()) currentTetromino = Array(Array((middle - 2, 0), (middle - 1, 0), (middle - 1, 1), (middle, 1)),
        Array((middle, 0), (middle, 1), (middle - 1, 1), (middle - 1, 2)),
        Array((middle, 2), (middle - 1, 2), (middle - 1, 1), (middle - 2, 1)),
        Array((middle - 2, 2), (middle - 2, 1), (middle - 1, 1), (middle - 1, 0)))
    else currentTetromino = Array(Array((middle - 1, 0), (middle, 0), (middle, 1), (middle + 1, 1)),
        Array((middle + 1, 0), (middle + 1, 1), (middle, 1), (middle, 2)),
        Array((middle + 1, 2), (middle, 2), (middle, 1), (middle - 1, 1)),
        Array((middle - 1, 2), (middle - 1, 1), (middle, 1), (middle, 0)))
    blockType = ZBlock
  }
}