package blockjumper

import org.scalajs.dom
import scala.concurrent.duration.*

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

  def doesCollect(powerUp: PowerUp): Boolean =
    Soldier.powerUpHitEdge.exists { (hitX, hitY) =>
      powerUp.eclipse.contains(hitX + x, hitY + y)
    }

  /** If the soldier is on either the left or right third, spawn on the other
    * side. If the soldier is on the left half, but moving to the left, spawn on
    * the right. If the soldier is on the right half, but moving to the right,
    * spawn on the left
    */
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
  // this is a sequence of points on the outline of the soldier image. The
  // coordinates are in the original, unscaled image. We divide them by 4
  // since we scale to 1/4 in this game, and then we flip over the center line
  // of the soldier so we have points on both sides
  def powerUpHitEdge: List[(Int, Int)] =
    val leftSide = List(
      (96, 0),
      (61, 7),
      (44, 37),
      (21, 97),
      (17, 152),
      (8, 234),
      (19, 309),
      (24, 335),
      (59, 335),
      (96, 335)
    ).map((x, y) => (x / 4, y / 4))
    val flipped = leftSide.map((x, y) => ((HitLine - x) + HitLine, y))
    leftSide ++ flipped

  val image =
    dom.document.createElement("img").asInstanceOf[dom.HTMLImageElement]
  image.src = "./soldier.png"
