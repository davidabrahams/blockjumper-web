package blockjumper

import org.scalajs.dom
import scala.concurrent.duration.*

case class Block(
    x: Double,
    width: Int,
    height: Int,
    movingDirection: LeftOrRight,
    colorOverride: Option[String],
    velocityOverride: Option[Double]
):
  def y: Int = GameState.GrassHeight - height
  private def area: Int = width * height
  private def velocity: Double =
    // simplified form of original code, multiplied by 50
    velocityOverride.getOrElse(700 - 0.09375 * area)
  def isOffScreen = (x + width < 0) || (x > GameState.ScreenWidth + width)
  def color: String =
    if colorOverride.isDefined then colorOverride.get
    else if area < 2849 then "#EEEE00"
    else if area < 4444 then "#FF1818"
    else "#0000E6"
  def update(timeElapsed: Duration): Block =
    val directionInt = movingDirection match
      case LeftOrRight.Left  => -1
      case LeftOrRight.Right => 1
    this.copy(x = x + directionInt * velocity * timeElapsed.toUnit(SECONDS))
  def explosionHitPoints: List[(Double, Double)] =
    val xCoords: List[Double] = List.range(0, 5).map(i => x + i * width / 4)
    val yCoords: List[Double] = List.range(0, 5).map(i => y + i * height / 4)
    xCoords.flatMap(xc => yCoords.map(yc => (xc, yc)))
  def shrink(factor: Int): Block =
    if factor == 1 then this
    else
      val newWidth = width / factor
      val newHeight = height / factor
      val newX = x + (width - newWidth) / 2
      this.copy(
        x = newX,
        width = newWidth,
        height = newHeight,
        colorOverride = Some(color),
        velocityOverride = Some(velocity)
      )

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
    Block(initialX, width, height, startLeftOrRight.flip, None, None)
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
