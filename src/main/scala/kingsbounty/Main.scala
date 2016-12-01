package kingsbounty

import kingsbounty.flow.{Banner, CommandExecutor, Loading, UserInterface}
import kingsbounty.ui.DrawingContext
import org.scalajs.dom
import org.scalajs.dom.html

import scala.language.implicitConversions
import scala.scalajs.js.annotation.JSExport
import scala.concurrent.ExecutionContext.Implicits.global

@JSExport
object Game {

  @JSExport
  def main(canvas: html.Canvas): Unit = {

    var currentInterface: UserInterface = Loading()

    val drawingContext = new DrawingContext(canvas.getContext("2d").asInstanceOf[dom.CanvasRenderingContext2D] )
    drawingContext.ready.map(_ => {
      currentInterface = Banner()
      drawingContext.draw(currentInterface)
    })

    dom.document.onkeydown = (e: dom.KeyboardEvent) => {
      currentInterface = CommandExecutor.executeCommand(currentInterface, e.keyCode)
      drawingContext.draw(currentInterface)
    }

    drawingContext.draw(currentInterface)
  }
}