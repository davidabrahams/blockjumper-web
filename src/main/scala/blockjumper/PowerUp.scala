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
    val rate: Double
):
  // 7.5, .1333 is correct value
  case SuperJump extends PowerUpInfo("Super\nJump", "#E84023", 0, 1)

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
    PowerUp.fillText(context, info.text, centerX, centerY)
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
      centerY: Double
  ): Unit =
    val lines = text.split("\n")
    val lineHeight = 12
    val apxFontHeight = lineHeight * lines.length
    // the text is drawn based on the bottom left coordinate. The logic makes
    // the center of the text in the y-axis equal centerY
    var currentY = centerY + lineHeight - apxFontHeight / 2
    context.fillStyle = "#FFFFFF" // white
    context.font = "14px sans-serif";
    lines.foreach { l =>
      context.fillText(l, centerX - context.measureText(l).width / 2, currentY)
      currentY += lineHeight
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
