import scala.io.Source
import InterfaceGraphique._
import java.awt.{Color, Dimension, Graphics, Graphics2D, Point, geom}
object Test{

def getColumn(col : Int, matrice : Array[Array[Float]]) : Array[Float] =
{
   var vect : Array[Float] = Array.ofDim(150)
   for(i <- 0 until 150) {
      vect(i) = matrice(i)(col)
   }
   return vect
}

def main(args: Array[String]) 
{
  	var k = new Kmeans()
  	var matrice = k.getMatrice()

  	k.affMatrice(matrice)
    val liste : List[String] = List("longeur sepale","largeur sepale","longeur petale","largeur petale")
    for(i <- 0 until 4)
    {
   		println(s"Moyenne ${liste(i)} ${i+1} : ${k.calcul_moyenne(matrice,i)}")
     	println(s"Variance ${liste(i)} ${i+1} : ${k.calcul_variance(matrice,i)}")
      	println(s"Écart-type ${liste(i)} ${i+1} : ${k.calcul_ecart_type(matrice,i)}")
      	if(i < 3)
      	{
        	println(s"coefficient_correlation ${liste(i)} ${i+1} avec ${i+2} : ${k.coeff_correl(getColumn(i,matrice),getColumn(i+1,matrice))} \n")
        }
        //println(s"Calcul_Distance ${liste(i)} ${i+1} : ${k.calcule_Distance(matrice,i)}")
    }
  }
}