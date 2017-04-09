package kasino

import scala.swing._
import javax.imageio.ImageIO
import java.io.File
import java.awt.image.BufferedImage
import java.awt.{Graphics2D, Color}


object KasinoGUI extends SimpleSwingApplication {
  
  def top = new MainFrame {
    title    = "Kasino"
    contents = new PlayArea
    size     = new Dimension(1280, 720)
  }
 
  class PlayArea extends Panel {
    background = new Color(39, 119, 20)
    
  }
  
}