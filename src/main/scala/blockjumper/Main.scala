package blockjumper

import org.scalajs.dom.*


object Main:
  def main(args: Array[String]): Unit =
    val foo = document.querySelector("canvas").asInstanceOf[html.Canvas]
    val context = foo.getContext("2d").asInstanceOf[CanvasRenderingContext2D]
    println("Hello world!")
