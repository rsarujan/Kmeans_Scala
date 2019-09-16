import scala.swing._
import scala.swing.event._
import scala.swing.Swing._
import scala.swing.{MainFrame, Panel, SimpleSwingApplication}
import java.awt.{Color, Dimension, Graphics, Graphics2D, Point, geom}

object InterfaceGraphique extends SimpleSwingApplication {
  def top = new MainFrame {
    title = "Projection"
    contents = new Panel{
    	background=Color.white
    	preferredSize=new Dimension(500,500)
    	focusable= true
    }

  }
}