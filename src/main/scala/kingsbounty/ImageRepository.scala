package kingsbounty

import org.scalajs.dom
import org.scalajs.dom.raw.{Event, HTMLImageElement}

import scala.concurrent.{Future, Promise}
import scala.concurrent.ExecutionContext.Implicits.global

class ImageRepository(canvasContext: dom.CanvasRenderingContext2D) {

  private def imageWithSrc(src: String): HTMLImageElement = {
    val image = dom.document.createElement("img").asInstanceOf[HTMLImageElement]
    image.src = src
    image
  }

  private def onLoadFuture(img: HTMLImageElement) = {
    if (img.complete) {
      Future.successful(img.src)
    } else {
      val p = Promise[String]()
      img.onload = { (e: Event) =>
        p.success(img.src)
      }
      p.future
    }
  }

  val banner = imageWithSrc("images/banner.jpg")
  val paladin = imageWithSrc("images/paladin.jpg")
  val warrior = imageWithSrc("images/warrior.jpg")
  val sorceress = imageWithSrc("images/sorceress.jpg")
  val barbarian = imageWithSrc("images/barbarian.jpg")
  val grass = imageWithSrc("images/grass.jpg")
  val water = imageWithSrc("images/water.jpg")
  val player = imageWithSrc("images/player.jpg")
  val wall = imageWithSrc("images/wall.jpg")
  val forest = imageWithSrc("images/forest.jpg")
  val castle = imageWithSrc("images/castle.jpg")
  val mountain = imageWithSrc("images/mountain.jpg")
  val sign = imageWithSrc("images/sign.jpg")
  val town = imageWithSrc("images/town.jpg")
  val knight = imageWithSrc("images/knight.jpg")
  val militia = imageWithSrc("images/militia.jpg")
  val archer = imageWithSrc("images/archer.jpg")
  val cavalier = imageWithSrc("images/cavalier.jpg")
  val pikeman = imageWithSrc("images/pikeman.jpg")

  val loaded: Future[Unit] = Future.sequence(Set(
    banner, paladin, warrior, sorceress, barbarian, grass, water, player,
    castle, forest, wall, mountain, sign, town, knight, militia, archer, cavalier,
    pikeman
  ).map(onLoadFuture(_))).map(_ => Unit)
}