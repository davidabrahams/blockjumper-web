package blockjumper

import org.scalajs.dom
import scala.concurrent.duration.*

case class Block(
    x: Double,
    width: Int,
    height: Int,
    movingDirection: LeftOrRight
):
  def y: Int = GameState.GrassHeight - height
  private def area: Int = width * height
  private def velocity: Double =
    700 - 0.09375 * area // simplified form of original code, multiplied by 50
  def isOffScreen = (x + width < 0) || (x > GameState.ScreenWidth + width)
  def color: String =
    if area < 2849 then "#EEEE00"
    else if area < 4444 then "#FF1818"
    else "#0000E6"
  def update(timeElapsed: Duration): Block =
    val directionInt = movingDirection match
      case LeftOrRight.Left  => -1
      case LeftOrRight.Right => 1
    Block(
      x + directionInt * velocity * timeElapsed.toUnit(SECONDS),
      width,
      height,
      movingDirection
    )
  def explosionHitPoints: List[(Double, Double)] =
    val xCoords: List[Double] = List.range(0, 5).map(i => x + i * width / 4)
    val yCoords: List[Double] = List.range(0, 5).map(i => y + i * height / 4)
    xCoords.flatMap(xc => yCoords.map(yc => (xc, yc)))

object Block:
  def spawnRate(totalGameTimeElapsedSeconds: Double) =
    2.0 / 3.0 // TODO: use time elapsed
  def generateRandom(rng: util.Random, spawnSide: Option[LeftOrRight]): Block =
    val width = (rng.nextDouble() * 40).toInt + 41
    val height = (rng.nextDouble() * 40).toInt + 41
    val startLeftOrRight: LeftOrRight = spawnSide.getOrElse(
      if rng.nextBoolean() then LeftOrRight.Left else LeftOrRight.Right
    )
    val initialX = startLeftOrRight match
      case LeftOrRight.Left  => 0 - width
      case LeftOrRight.Right => GameState.ScreenWidth + width
    Block(initialX, width, height, startLeftOrRight.flip)
  def drawBlocks(
      blocks: List[Block],
      context: dom.CanvasRenderingContext2D
  ): Unit =
    blocks.groupBy(_.color).foreach { (color, coloredBlocks) =>
      context.beginPath()
      context.fillStyle = color
      coloredBlocks.foreach(b => context.rect(b.x, b.y, b.width, b.height))
      context.fill()
    }
