import scala.io.Source
import scala.math.sqrt
   
object Kmeans2 {
  def ajout_tab(line:String, i:Int, matrice:Array[Array[String]]) { 
      matrice(i) = line.split(",")
    }
   
    def affiche_tableau(matrice:Array[Array[String]]) {
      for(v <- matrice)
  		{
  	      print("[ ")
          for(c <- v)
          {
          print(s"$c, ")
        }
        print(" ]\n")
  		}
   
    }
   
    def calcul_moyenne(matrice:Array[Array[String]], i:Int): Double= {
      var som = 0.0
      for(j <- 0 until 150)
      {
        som=som+(matrice(j)(i)).toFloat
      }
      return  som/150
    }
   
    def calcul_variance(matrice:Array[Array[String]], i:Int): Double= {
      var som = 0.0
      val moy = calcul_moyenne(matrice, i)
      for(j <- 0 until 150)
      {
        som=som+((matrice(j)(i)).toFloat - moy)*((matrice(j)(i)).toFloat - moy)
      }
      return som/150
    }
   
    def calcul_ecart_type(matrice:Array[Array[String]], i:Int): Double= {sqrt(calcul_variance(matrice, i))}
   
    def main(args: Array[String]) {
   
      var matrice=Array.ofDim[Array[String]](150)
      var i=0
      for (line <- Source.fromFile("iris.data.txt").getLines()) {
        ajout_tab(line, i, matrice)
        i=i+1
   
      }
      affiche_tableau(matrice)
      println("Moyenne longueur sétale :", calcul_moyenne(matrice, 0))
      println("Moyenne largeur sétale :", calcul_moyenne(matrice, 1))
      println("Moyenne longueur pétale :", calcul_moyenne(matrice, 2))
      println("Moyenne largeur pétale :", calcul_moyenne(matrice, 3))
   
      println("\n--------------------------------------------------------------------\n")
   
      println("Variance longueur sétale :", calcul_variance(matrice, 0))
      println("Variance largeur sétale :", calcul_variance(matrice, 1))
      println("Variance longueur pétale :", calcul_variance(matrice, 2))
      println("Variance largeur pétale :", calcul_variance(matrice, 3))
   
      println("\n--------------------------------------------------------------------\n")
   
      println("Écart-type longueur sétale :", calcul_ecart_type(matrice, 0))
      println("Écart-type largeur sétale :", calcul_ecart_type(matrice, 1))
      println("Écart-type longueur pétale :", calcul_ecart_type(matrice, 2))
      println("Écart-type largeur pétale :", calcul_ecart_type(matrice, 3))
   
      println("\n--------------------------------------------------------------------\n")
   
/*      println("coefficient_de_correlation longueur sétale :", calcul_coefficient_de_correlation(matrice, 0, 1))
      println("coefficient_de_correlation largeur sétale :", calcul_coefficient_de_correlation(matrice, 0, 2))
      println("coefficient_de_correlation longueur pétale :", calcul_coefficient_de_correlation(matrice, 0, 3))
      println("coefficient_de_correlation largeur pétale :", calcul_coefficient_de_correlation(matrice, 1, 2))*/
    }
  }