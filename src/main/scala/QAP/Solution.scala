package QAP

class Solution(var vals: Vector[Int], var cost: Int) extends Serializable {
 
  def this(vals:Vector[Int]) = this(vals, 0)


  def calculate_cost(instancia: Instance): Unit = {
    cost = 0

    for (i <- 0 until instancia.tam) {
      for (j <- 0 until instancia.tam) {
        cost += instancia.flujo(i)(j) * instancia.distancia(vals(i))(vals(j))
      }
    }
  }


  def factorize_cost(instancia: Instance, r: Int, s: Int): Int = {

    var deltaC: Int = 0

    for(k <- 0 until instancia.tam){
      if(k!=r && k!= s)
        deltaC += (
          instancia.flujo(r)(k) * (instancia.distancia(vals(s))(vals(k)) - instancia.distancia(vals(r))(vals(k)))
        + instancia.flujo(s)(k) * (instancia.distancia(vals(r))(vals(k)) - instancia.distancia(vals(s))(vals(k)))
        + instancia.flujo(k)(r) * (instancia.distancia(vals(k))(vals(s)) - instancia.distancia(vals(k))(vals(r)))
        + instancia.flujo(k)(s) * (instancia.distancia(vals(k))(vals(r)) - instancia.distancia(vals(k))(vals(s)))
          )

    }

    deltaC
  }
  
  def neightbor(instancia: Instance): Solution = {
    val r = scala.util.Random
    
    val ale1 = r.nextInt(instancia.tam)
    
    var ale2 = r.nextInt(instancia.tam)
      
    while(ale1 == ale2)  
      ale2 = r.nextInt(instancia.tam)


    val vecino = neightbor(instancia, ale1, ale2)
    
    vecino    
  }

  def neightbor(instancia: Instance, i: Int, j: Int): Solution = {

    var permutacion = vals
    val aux = permutacion(i)
    permutacion = permutacion.updated(i, permutacion(j))
    permutacion = permutacion.updated(j, aux)

    val vecino = new Solution(permutacion)
    vecino.cost = cost + factorize_cost(instancia, i, j)

    vecino
  }

  def neightborhood(instancia: Instance): List[Solution] = {
    val ret = Array.ofDim[Solution](instancia.tam * (instancia.tam - 1) / 2)
    var cont = 0
    for (i <- 0 until instancia.tam) {
      for (j <- i + 1 until instancia.tam) {
        ret(cont) = this.neightbor(instancia, i, j)
        cont += 1
      }
    }

    ret.toList
  }

  override def toString: String = {
    super.toString
    vals + " " + cost

  }

  def <(other: Solution): Boolean ={
    cost < other.cost
  }

  def >(other: Solution): Boolean ={
    cost > other.cost
  }
}

