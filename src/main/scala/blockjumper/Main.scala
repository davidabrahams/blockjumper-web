package blockjumper

import org.scalajs.dom.*

object Main:
  def main(args: Array[String]): Unit =
    val context: CanvasRenderingContext2D = document
      .querySelector("canvas")
      .asInstanceOf[html.Canvas]
      .getContext("2d")
      .asInstanceOf
    val canvas: HTMLCanvasElement = context.canvas
    canvas.width = 1170
    canvas.height = 900
    println("Hello word!")
