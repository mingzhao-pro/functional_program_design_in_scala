
object toto {

  val v1 = Vector(Vector('S', 'T'), Vector('o', 'o'), Vector('o', 'o'))
  def terrainFunction(levelVector: Vector[Vector[Char]]): Pos => Boolean =
    p => (levelVector flatMap (x => x.toList)) contains (levelVector(p.row-1)(p.col-1))

  v1 flatMap (_.toList)

  terrainFunction(v1)(Pos(1,2))

  val startPos = Pos(1,3)
  val goal = Pos(5, 8)
  val b = Block(startPos, startPos)

  lazy val pathsFromStart: Stream[(Block, List[Move])] = from(Stream((Block(startPos, startPos), List())), Set(Block(startPos, startPos)))

  lazy val pathsToGoal: Stream[(Block, List[Move])] = {
   for{
     path <- pathsFromStart
     if(path._1 == Block(goal, goal))
   } yield path
  }

//  (from(Stream((b, List())), Set(b)) take 15) toList

  def from(initial: Stream[(Block, List[Move])],
           explored: Set[Block]): Stream[(Block, List[Move])] = {

    val more = for {
      init <- initial
      nb <- newNeighborsOnly(neighborsWithHistory(init._1, init._2), explored)
      if !(explored contains nb._1)
    } yield nb

    initial #::: from(more, explored ++ more.map(_._1))
  }

  def neighborsWithHistory(b: Block, history: List[Move]): Stream[(Block, List[Move])] = {
    val neighbors = for {
      neighbor <- b legalNeighbors
    } yield (neighbor._1, neighbor._2 :: history)
    neighbors.toStream
  }

  def newNeighborsOnly(neighbors: Stream[(Block, List[Move])],
                       explored: Set[Block]): Stream[(Block, List[Move])] =
    neighbors filter (x => !(explored contains x._1))

  case class Pos(row: Int, col: Int) {
    /** The position obtained by changing the `row` coordinate by `d` */
    def deltaRow(d: Int): Pos = copy(row = row + d)

    /** The position obtained by changing the `col` coordinate by `d` */
    def deltaCol(d: Int): Pos = copy(col = col + d)
  }

  sealed abstract class Move
  case object Left  extends Move
  case object Right extends Move
  case object Up    extends Move
  case object Down  extends Move

  case class Block(b1: Pos, b2: Pos) {

    // checks the requirement mentioned above
    require(b1.row <= b2.row && b1.col <= b2.col, "Invalid block position: b1=" + b1 + ", b2=" + b2)

    /**
      * Returns a block where the `row` coordinates of `b1` and `b2` are
      * changed by `d1` and `d2`, respectively.
      */
    def deltaRow(d1: Int, d2: Int) = Block(b1.deltaRow(d1), b2.deltaRow(d2))

    /**
      * Returns a block where the `col` coordinates of `b1` and `b2` are
      * changed by `d1` and `d2`, respectively.
      */
    def deltaCol(d1: Int, d2: Int) = Block(b1.deltaCol(d1), b2.deltaCol(d2))

    /** The block obtained by moving left */
    def left = if (isStanding)             deltaCol(-2, -1)
    else if (b1.row == b2.row)  deltaCol(-1, -2)
    else                        deltaCol(-1, -1)

    /** The block obtained by moving right */
    def right = if (isStanding)            deltaCol(1, 2)
    else if (b1.row == b2.row) deltaCol(2, 1)
    else                       deltaCol(1, 1)

    /** The block obtained by moving up */
    def up = if (isStanding)               deltaRow(-2, -1)
    else if (b1.row == b2.row)    deltaRow(-1, -1)
    else                          deltaRow(-1, -2)

    /** The block obtained by moving down */
    def down = if (isStanding)             deltaRow(1, 2)
    else if (b1.row == b2.row)  deltaRow(1, 1)
    else                        deltaRow(2, 1)

    /**
      * Returns the list of blocks that can be obtained by moving
      * the current block, together with the corresponding move.
      */
    def neighbors: List[(Block, Move)] =
      (this left, Left) ::
        (this right, Right) ::
        (this up, Up) ::
        (this down, Down) ::
        Nil

    /**
      * Returns the list of positions reachable from the current block
      * which are inside the terrain.
      */
    def legalNeighbors: List[(Block, Move)] = neighbors filter (_._1 isLegal)

    /**
      * Returns `true` if the block is standing.
      */
    def isStanding: Boolean = b1.row == b2.row && b1.col == b2.col

    /**
      * Returns `true` if the block is entirely inside the terrain.
      */
    def isLegal: Boolean = b1.col >= 0 && b1.row >= 0
  }
}