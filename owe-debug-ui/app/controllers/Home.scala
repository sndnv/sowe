package controllers

import javax.inject._

import play.api.mvc._

@Singleton
class Home @Inject()(cc: ControllerComponents)(implicit assetsFinder: AssetsFinder) extends AbstractController(cc) {

  def root = Action {
    Ok(views.html.home())
  }

}
