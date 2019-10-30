import java.util.Scanner

object Calculator {
	//Métodos de la caluladora
    def plus(x:Int, y:Int) = x + y;//suma
    def minus(x:Int, y:Int) = x - y;//resta
    def over(x:Float, y:Float) = x / y;//División
    def times(x:Int, y:Int) = x*y;//Multiplicación
	def Casideri(f: Double => Double, x:Double, h:Double) = (f(x+h) -f(x))/h
	def fdex (x: Double) = x*x + 2*x + 1
 	def whichisbigger(x:Int,y:Int){ //Comparación
        if (x > y){
            println("The first one is bigger");
        }
        else{
            println("The second one is bigger");
        }
        
    }
    //def square(x:Int) = x*x;//Elevar al cuadrado
	def square = (x:Int) => x * x 
    def swampRows [T](arr:Array[T], r1:Int, r2:Int){//Función para alternar filas, usada en la función de abajo
		val tmp = arr(r1)
		arr(r1) = arr(r2)
		arr(r2) = tmp
	}
  //a: arreglo 2D de coeficientes de las varibles, y: arreglo
    def gaussSolver(a:Array[Array[Double]], y:Array[Double]):Array[Double]= {

		for(i <- 0 until a.length-1){
			val maxRow = (i+1 until a.length).foldLeft(i)((max, r) =>
				if(a(r)(i).abs > a(max)(i).abs) r else max)
			swampRows(a,maxRow,i)
			swampRows(y,maxRow,i)

			for(j <- i+1 until a.length){
				val factor = a(j)(i)/a(i)(i)
				a(j)(i) = 0.0
				for (k <- i+1 until a.length){
					a(j)(k) -= a(i)(k)*factor
	        		}	
				y(j) -= y(i)*factor
			}
		}

		val x = new Array[Double](a.length)
		for (i <- a.length-1 to 0 by -1){//De -1 en -1.
			x(i) = y(i)
		
			for(j <- i+1 until a.length) x(i) -= a(i)(j)*x(j)
			x(i) /= a(i)(i)
		}
		x//Return x

    }

  	
    //val fac = (x:Int) => fac(x-1: Int)
    def main(args: Array[String]) {  
		val A = Array( Array(4.0,-2.0,-2.0), Array(8.0,-5.0,1.0), Array(-2.0,7.0,-2.0));//Declarar arreglos
		val Y = Array(10.0,6.0,3.0)
		val respuesta = gaussSolver(A,Y)//Llamar función
        println(square(10))
		println(respuesta.mkString(""))
		println(Casideri(fdex,2, 5))
    }
}

