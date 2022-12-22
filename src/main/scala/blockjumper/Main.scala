package blockjumper

import org.scalajs.dom
import scala.concurrent.duration.*

case class GameState(soldier: Soldier):
  def update(timeElapsed: Duration, keyState: KeyState): GameState = GameState(
    soldier.update(timeElapsed, keyState)
  )
  def draw(context: dom.CanvasRenderingContext2D): Unit = soldier.draw(context)

object GameState:
  val ScreenWidth = 800
  val ScreenHeight = 600
  val GrassHeight = 420

case class Soldier(x: Double, y: Double, yVelocity: Double):
  def update(timeElapsed: Duration, keyState: KeyState): Soldier =
    val floor = GameState.GrassHeight - Soldier.Height
    val newY = Math.min(floor, y + yVelocity * timeElapsed.toUnit(SECONDS))
    val newX = (keyState.getLeftDown, keyState.getRightDown) match
      case (true, false) =>
        x - Soldier.WalkingSpeed * timeElapsed.toUnit(SECONDS)
      case (false, true) =>
        x + Soldier.WalkingSpeed * timeElapsed.toUnit(SECONDS)
      case _ => x
    Soldier(
      newX,
      newY,
      if newY == floor then {
        if keyState.getUpDown then -Soldier.JumpVelocity
        else 0
      } else yVelocity + Soldier.Gravity * timeElapsed.toUnit(SECONDS)
    )

  def draw(context: dom.CanvasRenderingContext2D): Unit =
    context.drawImage(
      Soldier.image,
      x,
      y,
      Soldier.Width,
      Soldier.Height
    )

object Soldier:
  val Gravity = 5000 // original code had 2, running at 50 fps. 5000 = 2 * 50^2
  val Width = 51
  val Height = 84
  val WalkingSpeed = 400 // original code had 8. 400 = 8 * 50
  val JumpVelocity = 1400 // original code had 28. 1400 = 28 * 50

  val image =
    dom.document.createElement("img").asInstanceOf[dom.HTMLImageElement]
  image.src = "./soldier.png"

def animate(
    context: dom.CanvasRenderingContext2D,
    previousFrameTimestamp: Option[Double],
    gameState: GameState,
    keyState: KeyState
): Double => Unit = { (msTimestamp: Double) =>
  val timeElapsed: Duration = previousFrameTimestamp match
    case None       => Duration.Zero
    case Some(prev) => Duration.fromNanos(1000000 * (msTimestamp - prev))
  assert(timeElapsed >= Duration.Zero)
  // draw the sky
  context.fillStyle = "#5FA6E7"
  context.fillRect(
    0,
    0,
    GameState.ScreenWidth,
    GameState.GrassHeight
  ) // x, y, width, height
  // draw the grass
  context.fillStyle = "#3DB91F"
  context.fillRect(
    0,
    GameState.GrassHeight,
    GameState.ScreenWidth,
    GameState.ScreenHeight - GameState.GrassHeight
  )
  gameState.draw(context)
  dom.window.requestAnimationFrame(
    animate(
      context,
      Some(msTimestamp),
      gameState.update(timeElapsed, keyState),
      keyState
    )
  )
}

class KeyState(
    private var leftDown: Boolean,
    private var rightDown: Boolean,
    private var upDown: Boolean
):
  def getLeftDown = leftDown
  def getRightDown = rightDown
  def getUpDown = upDown
  def processKeyEvent(e: dom.KeyboardEvent, pressedDown: Boolean): Unit =
    e.keyCode match
      case 37 => leftDown = pressedDown
      case 38 => upDown = pressedDown
      case 39 => rightDown = pressedDown
      case _  => ()

@main def main(): Unit =
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
      context,
      None,
      // TODO: maybe align based on the center hit line
      GameState(Soldier(GameState.ScreenWidth / 2 - Soldier.Width / 2, 0, 0)),
      keyState
    )
  )
