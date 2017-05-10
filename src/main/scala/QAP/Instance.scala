package QAP

/** Una instancia del problema QAP según la QAP Lib.
  * Una instancia consta de un tamaño n, una matriz de distancias nxn y una matriz de flujos nxn
  *
  */
import scala.collection.mutable.ListBuffer

class Instance extends Serializable{
  var tam: Int = 0
  var flujo: Array[Array[Int]] = _
  var distancia: Array[Array[Int]] = _

  /**
    *  Constructor que lee el fichero con las instancia
     * @param file Archivo que contiene los datos de la instancia de la QAP Lib
    */
  def this(file: String)= {
    this()
    read(file)
  }

  /**
    *  Lee el archivo de la QAP Lib y rellena los datos correspondientes
    * @param file Archivo que contiene los datos de la instancia de la QAP Lib
    */
  def read(file: String){


    //Leo el fichero y obetengo todos los valores que contiene
    val fileLines = scala.io.Source.fromFile(file).getLines().map(_.split(" ").toVector).map(_.filter(a => !a.contentEquals(""))).filter(_.nonEmpty).toVector
    //Paso todos los valores a un único vector
    var aux = fileLines.reduce((a,b) => a++b).map(_.toInt)
    tam = aux.head // El primero es el tamaño del problema
    aux = aux.drop(1) // Lo elimino porque ya lo he leido

    //Ahora viene la matriz de ditancias que será de tamañao tam*tam
    distancia = Array.ofDim[Int](tam, 0)
    for(i <- 0 until tam){
      distancia(i) = aux.take(tam).toArray
      aux = aux.drop(tam)
    }

    flujo = Array.ofDim[Int](tam, 0)
    for(i <- 0 until tam){
      flujo(i) = aux.take(tam).toArray
      aux = aux.drop(tam)
    }

  }

  /**
    *  Genera una solución aleatoria de la instancia
    * @return La solución aleatoria generada
    */
  def Random_sol(): Solution = {
    val permutacion: Vector[Int] = util.Random.shuffle(0 to tam-1).toVector

    val sol = new Solution(permutacion)
    sol.calculate_cost(this)

    sol
  }

  /**
    *  Genera n soluciones aleatorias de la instancia
    *  @param n Número de soluciones aleaorias a generar
    * @return Una lista con las soluciones generadas
    */
  def Random_sol(n: Int): List[Solution] = {
    val buf = new ListBuffer[Solution]
    for(_ <- 0 until tam) {
      buf.prepend(Random_sol())
    }

    buf.toList
  }

  /** Sobrecarga del método toString para mostrar los datos de la instancia
    *
    *  @return String con el tamaño y las matrices de distancia y flujos
    */
  override def toString: String = {
    super.toString
    var aux: String = tam.toString + "\n\nDistancia:\n"

    for(i <- 0 until tam){
      for(j <- 0 until tam){
        aux += distancia(i)(j) + " "
      }

      aux += "\n"
    }

    aux += "Flujo:\n"

    for(i <- 0 until tam){
      for(j <- 0 until tam){
        aux += flujo(i)(j) + " "
      }

      aux += "\n"
    }

    aux
  }

}