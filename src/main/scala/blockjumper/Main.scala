package blockjumper

import org.scalajs.dom
import scala.concurrent.duration.*

enum LeftOrRight:
  case Left, Right
  def flip = this match
    case Left  => Right
    case Right => Left

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
