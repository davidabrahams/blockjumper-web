package blockjumper

import org.scalajs.dom
import scala.concurrent.duration.*

case class GameState(soldier: Soldier, blocks: List[Block]):
  def update(
      totalGameTimeSeconds: Double,
      timeElapsedSinceLastFrame: Duration,
      keyState: KeyState,
      rng: util.Random
  ): GameState =
    val newBlockSpawnOdds = Block.spawnRate(
      totalGameTimeSeconds
    ) * timeElapsedSinceLastFrame.toUnit(SECONDS)
    val maybeNewBlock: Option[Block] =
      if rng.nextDouble() < newBlockSpawnOdds
      then Some(Block.generateRandom(rng, soldier.getSpawnSide(keyState)))
      else None
    val newBlocks = maybeNewBlock.toList ++ blocks.filterNot(_.isOffScreen)
    GameState(
      soldier.update(timeElapsedSinceLastFrame, keyState),
      newBlocks.map(_.update(timeElapsedSinceLastFrame))
    )
  def draw(context: dom.CanvasRenderingContext2D): Unit =
    Block.drawBlocks(blocks, context)
    soldier.draw(context)
  def isOver: Boolean = blocks.exists(block => soldier.isHit(block))

object GameState:
  val ScreenWidth = 800
  val ScreenHeight = 600
  val GrassHeight = 420

case class Soldier(x: Double, y: Double, yVelocity: Double):
  def update(timeElapsed: Duration, keyState: KeyState): Soldier =
    val floor = GameState.GrassHeight - Soldier.Height
    val newY = Math.min(floor, y + yVelocity * timeElapsed.toUnit(SECONDS))
    val xAfterWalking = (keyState.getLeftDown(), keyState.getRightDown()) match
      case (true, false) =>
        x - Soldier.WalkingSpeed * timeElapsed.toUnit(SECONDS)
      case (false, true) =>
        x + Soldier.WalkingSpeed * timeElapsed.toUnit(SECONDS)
      case _ => x
    // force the soldier to stay in bounds
    val newX = Math.min(
      Math.max(xAfterWalking, -Soldier.LeftEdge),
      GameState.ScreenWidth - Soldier.RightEdge
    )
    Soldier(
      newX,
      newY,
      if newY == floor then
        if keyState.getUpDown() then -Soldier.JumpVelocity
        else 0
      else yVelocity + Soldier.Gravity * timeElapsed.toUnit(SECONDS)
    )

  def draw(context: dom.CanvasRenderingContext2D): Unit =
    context.drawImage(
      Soldier.image,
      x,
      y,
      Soldier.Width,
      Soldier.Height
    )

  private def hitPointX = x + Soldier.HitLine

  def isHit(block: Block): Boolean =
    val hitPointY = y + Soldier.Height
    val xHit = hitPointX > block.x && hitPointX < block.x + block.width
    // the soldier's foot is roughly 18 pixels wide. Because we only check his
    // center line for collision, we also give an 18 pixel buffer on the top
    // of blocks to make it feel consistent. The second check comparison has to
    // be <= because when the soldier sits on the ground,
    // hitPointY == block.y + block.height
    val yHit = hitPointY > block.y + 18 && hitPointY <= block.y + block.height
    xHit && yHit

  def getSpawnSide(keyState: KeyState): Option[LeftOrRight] =
    val leftHalf = hitPointX < GameState.ScreenWidth / 2
    val rightHalf = !leftHalf
    val leftThird = hitPointX < GameState.ScreenWidth / 3
    val rightThird = hitPointX > 2 * GameState.ScreenWidth / 3
    if leftThird then Some(LeftOrRight.Right)
    else if rightThird then Some(LeftOrRight.Left)
    else if leftHalf && keyState.getLeftDown() && !keyState.getRightDown() then
      Some(LeftOrRight.Right)
    else if rightHalf && !keyState.getLeftDown() && keyState.getRightDown() then
      Some(LeftOrRight.Left)
    else None

object Soldier:
  val Gravity = 5000 // original code had 2, running at 50 fps. 5000 = 2 * 50^2
  val HitLine = 24 // roughly the mid line of the soldier image
  val Width = 51
  val Height = 84
  val WalkingSpeed = 400 // original code had 8. 400 = 8 * 50
  val JumpVelocity = 1400 // original code had 28. 1400 = 28 * 50
  val LeftEdge = 2 // the soldier's left hand in the image
  val RightEdge = 46 // the soldier's right hand in the image

  val image =
    dom.document.createElement("img").asInstanceOf[dom.HTMLImageElement]
  image.src = "./soldier.png"

enum LeftOrRight:
  case Left, Right
  def flip = this match
    case Left  => Right
    case Right => Left

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
    if area < 2849 then "EEEE00"
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

def animate(
    rng: util.Random,
    context: dom.CanvasRenderingContext2D,
    previousFrameTimestamp: Option[Double],
    gameState: GameState,
    keyState: KeyState
): Double => Unit = { (msTimestamp: Double) =>
  val timeElapsed: Duration = previousFrameTimestamp match
    case None       => Duration.Zero
    case Some(prev) => Duration.fromNanos(1000000 * (msTimestamp - prev))
  assert(timeElapsed >= Duration.Zero)
  context.beginPath()
  // draw the sky
  context.fillStyle = "#5FA6E7"
  context.rect(
    0,
    0,
    GameState.ScreenWidth,
    GameState.GrassHeight
  ) // x, y, width, height
  context.fill()
  // draw the grass
  context.beginPath()
  context.fillStyle = "#3DB91F"
  context.rect(
    0,
    GameState.GrassHeight,
    GameState.ScreenWidth,
    GameState.ScreenHeight - GameState.GrassHeight
  )
  context.fill()
  gameState.draw(context)
  if !gameState.isOver then
    dom.window.requestAnimationFrame(
      animate(
        rng,
        context,
        Some(msTimestamp),
        gameState.update(msTimestamp, timeElapsed, keyState, rng),
        keyState
      )
    )
}

class KeyState(
    private var leftDown: Boolean,
    private var rightDown: Boolean,
    private var upDown: Boolean
):
  def getLeftDown() = leftDown
  def getRightDown() = rightDown
  def getUpDown() = upDown
  def processKeyEvent(e: dom.KeyboardEvent, pressedDown: Boolean): Unit =
    e.keyCode match
      case 37 => leftDown = pressedDown
      case 38 => upDown = pressedDown
      case 39 => rightDown = pressedDown
      case _  => ()

@main def main(): Unit =
  val rng = util.Random()
  val context: dom.CanvasRenderingContext2D = dom.document
    .querySelector("canvas")
    .asInstanceOf[dom.html.Canvas]
    .getContext("2d")
    .asInstanceOf
  val canvas: dom.HTMLCanvasElement = context.canvas
  canvas.width = GameState.ScreenWidth
  canvas.height = GameState.ScreenHeight
  val keyState = KeyState(false, false, false)
  dom.window.addEventListener("keydown", e => keyState.processKeyEvent(e, true))
  dom.window.addEventListener("keyup", e => keyState.processKeyEvent(e, false))
  dom.window.requestAnimationFrame(
    animate(
      rng,
      context,
      None,
      // TODO: maybe align based on the center hit line
      GameState(
        Soldier(GameState.ScreenWidth / 2 - Soldier.Width / 2, 0, 0),
        List.empty
      ),
      keyState
    )
  )
