package blockjumper

import org.scalajs.dom
import scala.concurrent.duration.*

case class GameState(soldier: Soldier):
  def update(timeElapsed: Duration): GameState = GameState(
    soldier.update(timeElapsed)
  )
  def draw(context: dom.CanvasRenderingContext2D): Unit = soldier.draw(context)

case class Soldier(x: Double, y: Double, yVelocity: Double):
  def update(timeElapsed: Duration): Soldier =
    val afterFall = y + yVelocity * timeElapsed.toUnit(SECONDS)
    if (afterFall > 420 - 84)
      Soldier(
        x,
        420-84,
        0
      )
    else
      Soldier(
        x,
        afterFall,
        yVelocity + Soldier.Gravity * timeElapsed.toUnit(SECONDS)
      )

  def draw(context: dom.CanvasRenderingContext2D): Unit =
    context.drawImage(
      Soldier.image,
      x,
      y,
      51,
      84
    )

object Soldier:
  val Gravity = 5000 // original code had 2, running at 50 fps. 5000 = 2 * 50^2
  val image =
    dom.document.createElement("img").asInstanceOf[dom.HTMLImageElement]
  image.src = "./soldier.png"

def animate(
    context: dom.CanvasRenderingContext2D,
    previousFrameTimestamp: Option[Double],
    gameState: GameState
): Double => Unit = { (msTimestamp: Double) =>
  val timeElapsed: Duration = previousFrameTimestamp match
    case None       => Duration.Zero
    case Some(prev) => Duration.fromNanos(1000000 * (msTimestamp - prev))
  assert(timeElapsed >= Duration.Zero)
  // draw the sky
  context.fillStyle = "#5FA6E7"
  context.fillRect(0, 0, 800, 420) // x, y, width, height
  // draw the grass
  context.fillStyle = "#3DB91F"
  context.fillRect(0, 420, 800, 180)
  gameState.draw(context)
  dom.window.requestAnimationFrame(
    animate(context, Some(msTimestamp), gameState.update(timeElapsed))
  )
}

@main def main(): Unit =
  val context: dom.CanvasRenderingContext2D = dom.document
    .querySelector("canvas")
    .asInstanceOf[dom.html.Canvas]
    .getContext("2d")
    .asInstanceOf
  val canvas: dom.HTMLCanvasElement = context.canvas
  canvas.width = 800
  canvas.height = 600
  dom.window.requestAnimationFrame(
    animate(context, None, GameState(Soldier(390, 0, 0)))
  )
