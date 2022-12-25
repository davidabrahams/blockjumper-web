package blockjumper

import org.scalajs.dom
import scala.concurrent.duration.*

/** @param firstAppear
  *   when the power up first appears, in seconds
  * @param rate
  *   how often it spawns, in count/second
  */
enum PowerUpInfo(
    val text: String,
    val color: String,
    val firstAppear: Double,
    val rate: Double,
    val fontSize: Int,
    val fontColor: String
):
  // 7.5, .1333 is correct
  case SuperJump
      extends PowerUpInfo("Super\nJump", "#E84023", 0, 1, 14, "#FFFFFF")
  // 15, .0667 is correct
  case Invincibility
      extends PowerUpInfo("Invincibility", "#D2FBCE", 0, 1, 10, "#000000")

case class PowerUp(centerX: Double, centerY: Double, info: PowerUpInfo):
  def update(timeElapsed: Duration): PowerUp =
    this.copy(centerY =
      centerY + PowerUp.Velocity * timeElapsed.toUnit(SECONDS)
    )
  def isOffScreen = centerY - PowerUp.Radius > GameState.GrassHeight
  def draw(context: dom.CanvasRenderingContext2D): Unit =
    context.beginPath()
    context.arc(centerX, centerY, PowerUp.Radius, 0, 2 * math.Pi)
    context.fillStyle = info.color
    context.fill()
    PowerUp.fillText(
      context,
      info.text,
      centerX,
      centerY,
      info.fontSize,
      info.fontColor
    )
  def eclipse = Eclipse(
    centerX - PowerUp.Radius,
    centerY - PowerUp.Radius,
    PowerUp.Radius * 2,
    PowerUp.Radius * 2
  )

object PowerUp:
  val Radius = 25
  val Velocity = 200 // original code had 4
  private def fillText(
      context: dom.CanvasRenderingContext2D,
      text: String,
      centerX: Double,
      centerY: Double,
      fontSize: Int,
      fontColor: String
  ): Unit =
    val fontLineHeight = fontSize - 2
    val lines = text.split("\n")
    val apxFontHeight = fontLineHeight * lines.length
    // the text is drawn based on the bottom left coordinate. The logic makes
    // the center of the text in the y-axis equal centerY
    var currentY = centerY + fontLineHeight - apxFontHeight / 2
    context.fillStyle = fontColor
    context.font = s"${fontSize}px sans-serif";
    lines.foreach { l =>
      context.fillText(l, centerX - context.measureText(l).width / 2, currentY)
      currentY += fontLineHeight
    }

  def spawnPowerUps(
      rng: util.Random,
      timeElapsed: Duration,
      totalGameTimeSeconds: Double
  ): List[PowerUp] =
    PowerUpInfo.values.collect {
      case powerUpInfo
          if totalGameTimeSeconds > powerUpInfo.firstAppear && rng
            .nextDouble() < powerUpInfo.rate * timeElapsed.toUnit(SECONDS) =>
        PowerUp(
          // valid centerX range is [radius, width - radius]
          rng.nextDouble * (GameState.ScreenWidth - 2 * Radius) + Radius,
          // start the power up completely off screen
          -Radius,
          powerUpInfo
        )
    }.toList
