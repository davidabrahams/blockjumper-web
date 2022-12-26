package blockjumper

import org.scalajs.dom
import scala.concurrent.duration.*

case class GameState(
    soldier: Soldier,
    blocks: List[Block],
    powerUps: List[PowerUp],
    bullets: List[Bullet]
):

  def update(
      totalGameTimeSeconds: Double,
      timeElapsedSinceLastFrame: Duration,
      keyState: KeyState,
      rng: util.Random
  ): GameState =
    // shadow the soldier variable here to the version which has collected all the power ups
    val (soldier, collectedPowerUps) = this.soldier.collectPowerUps(powerUps)
    val newBlockSpawnOdds = Block.spawnRate(
      totalGameTimeSeconds
    ) * timeElapsedSinceLastFrame.toUnit(SECONDS)
    val maybeNewBlock: Option[Block] =
      if rng.nextDouble() < newBlockSpawnOdds
      then Some(Block.generateRandom(rng, soldier.getSpawnSide(keyState)))
      else None
    val maybeNewBullet: Option[Bullet] =
      if keyState.processXClick() then soldier.maybeSpawnBullet else None
    val doExplode = keyState.processZClick() && soldier.explosions > 0
    val allBlocksDestroyed =
      collectedPowerUps.contains(PowerUpInfo.DestroyAllBlocks)
    val shrinkBy = shrinkFactor(collectedPowerUps)
    GameState(
      soldier
        .copy(
          bullets =
            soldier.bullets - (if maybeNewBullet.isDefined then 1 else 0),
          explosions = soldier.explosions - (if doExplode then 1 else 0),
          explosionSecondsRemaining =
            if doExplode then 0.4 else soldier.explosionSecondsRemaining
        )
        .completeJumps
        .applyKeyPresses(keyState)
        .applyJumps
        .update(timeElapsedSinceLastFrame),
      (maybeNewBlock.toList ++ blocks)
        .filterNot(_ => allBlocksDestroyed)
        .filterNot(soldier.explodedBlock)
        .filterNot(_.isOffScreen)
        .filterNot(block => bullets.exists(_.hit(block)))
        .map(_.shrink(shrinkBy))
        .map(_.update(timeElapsedSinceLastFrame)),
      (PowerUp.spawnPowerUps(
        rng,
        timeElapsedSinceLastFrame,
        totalGameTimeSeconds
      ) ++ powerUps)
        .filterNot(soldier.doesCollect)
        .filterNot(_.isOffScreen)
        .map(_.update(timeElapsedSinceLastFrame)),
      (maybeNewBullet.toList ++ bullets)
        .filterNot(_.isOffScreen)
        .map(_.update(timeElapsedSinceLastFrame))
    )

  def draw(context: dom.CanvasRenderingContext2D): Unit =
    Block.drawBlocks(blocks, context)
    powerUps.foreach(_.draw(context))
    Bullet.drawBullets(bullets, context)
    soldier.draw(context)
    drawPowerUpCounters(context)

  def isOver: Boolean = blocks.exists(block => soldier.isHit(block))

  private def drawPowerUpCounters(context: dom.CanvasRenderingContext2D): Unit =
    Util.drawCircleWithText(
      context,
      740,
      60,
      50,
      "#E84023",
      soldier.superJumps.toString,
      30,
      "#FFFFFF"
    )

  private def shrinkFactor(collectedPowerUps: List[PowerUpInfo]): Int =
    collectedPowerUps.foldLeft(1) {
      case (i, PowerUpInfo.ShrinkAllBlocks) => i * 2
      case (i, _)                           => i
    }

object GameState:
  val ScreenWidth = 800
  val ScreenHeight = 600
  val GrassHeight = 420
