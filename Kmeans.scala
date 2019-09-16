import scala.io.Source
import scala.math.sqrt
import scala.math.pow
class Kmeans{
    private val filename = "iris.data.txt"
    private val matrice = matriceToFloat(setupMatrice())

	//Initialise la matrice
    def setupMatrice() : Array[Array[String]] = {
      var tab : Array[Array[String]]=Array.ofDim[String](Source.fromFile(filename).getLines.size,Source.fromFile(filename).getLines.count(_ == ',')+1)
      var i = 0
      for (line <- Source.fromFile(filename).getLines)
      	{
      		tab(i)=line split(",")
      		i=i+1
      	}
      	return tab
      }


	//convertit la matrice en float
	def matriceToFloat(matrice : Array[Array[String]]) : Array[Array[Float]] = {
		var tabfl : Array[Array[Float]]=Array.ofDim[Float](matrice.size, matrice(0).size-1)
      	for(y <- 0 to tabfl.size -1)
      	{
        	for(x <- 0 to tabfl(0).size-1)
        	{
          		tabfl(y)(x) = matrice(y)(x).toFloat
        	}
      	}
      	return tabfl
    }


	//affiche la matrice donnee
    def affMatrice(mat : Array[Array[Float]]) : Unit = {
      var chaine : String = ""
      for(y <- 0 to mat.size -1){
          for(x <- 0 to mat(0).size-1){
            chaine = chaine + "[" + mat(y)(x) + "]"
          }
      chaine = chaine + ("\n")
      }
      println(chaine)
     }


    //getter de la matrice
    def getMatrice() : Array[Array[Float]] = {
      return this.matrice
    }


/*****************************************************************************/
    //calcul moyenne de la colonne i
    def calcul_moyenne(matrice:Array[Array[Float]], i : Int): Double= {
        var som = 0.0
        for(j <- 0 until Source.fromFile(filename).getLines.size)
        {
          som=som+(matrice(j)(i))
        }
        return  som/Source.fromFile(filename).getLines.size
    }


    //calcul de la variance de la colonne i
    def calcul_variance(matrice:Array[Array[Float]], i : Int): Double= {
    var som = 0.0
    val moy = calcul_moyenne(matrice,i)
        for(j <- 0 until Source.fromFile(filename).getLines.size)
        {
          som=som+((matrice(j)(i)) - moy)*((matrice(j)(i)) - moy)
        }
        return som/Source.fromFile(filename).getLines.size
    }


    //ecart-type de la colonne i
    def calcul_ecart_type(matrice:Array[Array[Float]], i : Int): Double= {
        sqrt(calcul_variance(matrice,i))
    }


    /*      VERSION 2       */
    def calcul_moyenne(matrice:Array[Float]): Double = {
        var som = 0.0
            for(j <- 0 until matrice.size)
            {
                som=som+(matrice(j))
            }
        return  som/matrice.size
    }


    def calcul_variance(matrice:Array[Float]): Double = {
        var som = 0.0
        val moy = calcul_moyenne(matrice)
            for(j <- 0 until matrice.size)
            {
                som=som+((matrice(j)) - moy)*((matrice(j)) - moy)
            }
        return som/matrice.size
    }


    def calcul_ecart_type(matrice:Array[Float]): Double = {
        sqrt(calcul_variance(matrice))
    }


    //calcul de la covariance utilisé pour le coeff de coeff_correlation
    def covariance(x : Array[Float], y : Array[Float]) : Double = {
    var XY : Array[Float] =Array.ofDim(x.size)
        for(j <- 0 until x.size)
        {
        	XY(j) = (x(j)*y(j))
        }
        return calcul_moyenne(XY)-(calcul_moyenne(x)*calcul_moyenne(y))
    }


	//coefficient de correlation des deux colonnes x et y
    def coeff_correl(x :  Array[Float],y : Array[Float]) : Double = {
        return (covariance(x,y))/(calcul_ecart_type(x)*calcul_ecart_type(y))
    }


    def simulpoints(x : Array[Float], y : Array[Float]) : Array[Array[Float]] = {
		var matricePoint : Array[Array[Float]] = Array.ofDim[Float](x.size, 2)
		for(i <- 0 until x.size){
			matricePoint(i)(0) = (x(i) * 20).toInt
			matricePoint(i)(1) = (y(i) * 20).toInt
		}
		return matricePoint
	}


	//calcul de la distance entre deux tableaux de points
	def calcule_Distance(x : Array[Float], y : Array[Float], k : Int) : Float = {
		var distance : Float = 1
		for (i <- 0 until k){
			distance = distance + (pow((x(i) - y(i)),2)).toFloat
		}
		distance = sqrt((1/k.toFloat)*distance).toFloat
		println(distance)
		return distance
	}


	def listeDist(matrice : Array[Array[Float]], k : Int) : Array[Float] = {
		var listdist : Array[Float] = Array.ofDim(matrice.size)
		for (i <- 0 until listdist.size-1){
			listdist(i) = calcule_Distance(matrice(i),matrice(i+1),k)
		}
		return listdist
	}
}