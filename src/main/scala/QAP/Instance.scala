package QAP

import scala.collection.mutable.ListBuffer

class Instance extends Serializable{
  var tam: Int = 0
  var flujo: Array[Array[Int]] = _
  var distancia: Array[Array[Int]] = _
  
  def this(file: String)= {
    this()
    read(file)
  }
  
  def read(file: String){
    val fileLines = scala.io.Source.fromFile(file).getLines().toList
    //La primera línea es el tamaño del problema
    tam = fileLines.head.replace(" ", "").toInt
    
    //En la tercera empieza la matriz de flujos
    distancia = Array.ofDim[Int](tam, 0)
    for (i <- 0 until tam){
      val flu = fileLines(i + 2).split(" ")
      
      for (line <- flu)
        if(line.length() != 0) {
          distancia(i) :+= line.toInt
        }

    }
    
    //Al acabar la matriz de flujos hay un espacio en 
    //blanco, después la matriz de distancias
    flujo = Array.ofDim[Int](tam, 0)
    for (i <- 0 until tam){
      val dis = fileLines(tam + i + 3).split(" ")
      
      for (line <- dis)
        if(line.length() != 0){
          flujo(i) :+= line.toInt
        }

    }

  }


  
  def Random_sol(): Solution = {
    val permutacion: Vector[Int] = util.Random.shuffle(0 until tam).toVector
   
    val sol = new Solution(permutacion)
    sol.calculate_cost(this)
    
    sol
  }

  def Random_sol(n: Int): List[Solution] = {
    val buf = new ListBuffer[Solution]
    for(_ <- 0 until tam) {
      val permutacion: Vector[Int] = util.Random.shuffle(0 until tam).toVector

      val sol = new Solution(permutacion)
      sol.calculate_cost(this)
      buf += sol
    }

    buf.toList


  }
  
}