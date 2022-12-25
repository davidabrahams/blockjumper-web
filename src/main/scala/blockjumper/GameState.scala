package blockjumper

import org.scalajs.dom
import scala.concurrent.duration.*

case class GameState(
    soldier: Soldier,
    blocks: List[Block],
    powerUps: List[PowerUp]
):
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
    GameState(
      soldier
        .collectPowerUps(powerUps)
        .completeJumps
        .applyKeyPresses(keyState)
        .applyJumps
        .update(timeElapsedSinceLastFrame),
      (maybeNewBlock.toList ++ blocks)
        .filterNot(_.isOffScreen)
        .map(_.update(timeElapsedSinceLastFrame)),
      (PowerUp.spawnPowerUps(
        rng,
        timeElapsedSinceLastFrame,
        totalGameTimeSeconds
      ) ++ powerUps)
        .filterNot(soldier.doesCollect)
        .filterNot(_.isOffScreen)
        .map(_.update(timeElapsedSinceLastFrame))
    )

  def draw(context: dom.CanvasRenderingContext2D): Unit =
    Block.drawBlocks(blocks, context)
    powerUps.foreach(_.draw(context))
    soldier.draw(context)
  def isOver: Boolean = blocks.exists(block => soldier.isHit(block))

object GameState:
  val ScreenWidth = 800
  val ScreenHeight = 600
  val GrassHeight = 420
