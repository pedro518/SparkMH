package QAP

import scala.collection.mutable.ListBuffer

class Instance extends Serializable{
  var tam: Int = 0
  var flujo: Array[Array[Int]] = null
  var distancia: Array[Array[Int]] = null
  
  def this(file: String)= {
    this()
    read(file)
  }
  
  def read(file: String){
    val fileLines = scala.io.Source.fromFile(file).getLines().toList
    //La primera línea es el tamaño del problema
    tam = fileLines(0).replace(" ", "").toInt
    
    //En la tercera empieza la matriz de flujos
    distancia = Array.ofDim[Int](tam, 0)
    for (i <- 0 to tam-1){
      var flu = fileLines(i+2).split(" ")
      
      for (line <- flu)
        if(line.length() != 0) {
          distancia(i) :+= line.toInt
        }

    }
    
    //Al acabar la matriz de flujos hay un espacio en 
    //blanco, después la matriz de distancias
    flujo = Array.ofDim[Int](tam, 0)
    for (i <- 0 to tam-1){
      var dis = fileLines(tam+i+3).split(" ")
      
      for (line <- dis)
        if(line.length() != 0){
          flujo(i) :+= line.toInt
        }

    }

  }


  
  def Random_sol(): Solution = {
    val permutacion: Vector[Int] = util.Random.shuffle(0 to tam-1).toVector
   
    val sol = new Solution(permutacion)
    sol.calculate_cost(this)
    
    sol
  }

  def Random_sol(n: Int): List[Solution] = {
    val buf = new ListBuffer[Solution]
    for(i <- 0 to tam - 1) {
      val permutacion: Vector[Int] = util.Random.shuffle(0 to tam - 1).toVector

      val sol = new Solution(permutacion)
      sol.calculate_cost(this)
      buf += sol
    }

    buf.toList


  }
  
}