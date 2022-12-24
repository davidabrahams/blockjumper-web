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
    GameState(
      soldier.update(timeElapsedSinceLastFrame, keyState),
      (maybeNewBlock.toList ++ blocks)
        .map(_.update(timeElapsedSinceLastFrame))
        .filterNot(_.isOffScreen)
    )
  def draw(context: dom.CanvasRenderingContext2D): Unit =
    Block.drawBlocks(blocks, context)
    soldier.draw(context)
  def isOver: Boolean = blocks.exists(block => soldier.isHit(block))

object GameState:
  val ScreenWidth = 800
  val ScreenHeight = 600
  val GrassHeight = 420
