package QAP

  /** Una solución del problema QAP según la QAP Lib.
    * Una solución consta de una permutación de números enteros y un costo asociado a dicha permutación
    *
    *  @constructor crea una solución de QAP.
    *  @param vals Permutación de la solución
    *  @param cost Coste de la solución
    *
    */
class Solution(var vals: Vector[Int], var cost: Int) extends Serializable {

  /** Creates a person with a given name and age.
    *
    *  @param vals Permutación que forma la solución
    */
  def this(vals:Vector[Int]) = this(vals, 0)

  /** Calcula el costo de una solución.
    *
    *  @param instancia Instancia de la que forma parte la solución
    */
  def calculate_cost(instancia: Instance): Unit = {
    cost = 0

    for (i <- 0 until instancia.tam) {
      for (j <- 0 until instancia.tam) {
        cost += instancia.flujo(i)(j) * instancia.distancia(vals(i))(vals(j))
      }
    }
  }

  /** Calcula la diferencia de costo de una solución usando la factorización de esta a partir de la solución vecina de la que proviene.
    * En lugar de tener que hacer todos los cálculos, solo se resta el costo de las dos asignaciones a cambiar y se suma el de las nuevas.
    *
    *  @param instancia Instancia del problema de la que forma parte la solución
    *  @param r Una localización a intercambiar
    *  @param s Otra localización a intecarmbiar
    *  @return La diferencia de costos entre esta solución y la generada al intercambiar r y s
    */
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

  /** Genera un vecino aleatorio de esta solución
    *
    *  @param instancia Instancia del problema de la que forma parte la solución
    *  @return Una nueva solución vecina a la actual
    */
  def neightbor(instancia: Instance): Solution = {
    val r = scala.util.Random
    
    val ale1 = r.nextInt(instancia.tam)
    
    var ale2 = r.nextInt(instancia.tam)
      
    while(ale1 == ale2)  
      ale2 = r.nextInt(instancia.tam)


    val vecino = neightbor(instancia, ale1, ale2)
    
    vecino    
  }

  /** Genera el vecino especificado de esta solución
    *
    *  @param instancia Instancia del problema de la que forma parte la solución
    *  @param i Una localización a intercambiar
    *  @param j Otra locaclización a intercamviar
    *  @return La solución especificada al intercambiar i y j
    */
  def neightbor(instancia: Instance, i: Int, j: Int): Solution = {

    var permutacion = vals
    val aux = permutacion(i)
    permutacion = permutacion.updated(i, permutacion(j))
    permutacion = permutacion.updated(j, aux)

    val vecino = new Solution(permutacion)
    vecino.cost = cost + factorize_cost(instancia, i, j)

    vecino
  }

   /** Genera el vecindario completo de esta solución
     *
     *  @param instancia Instancia del problema de la que forma parte la solución
     *  @return Lista con todas las soluciones vecinas de esta solución
     */
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

  /** Sobrecarga del método toString para mostrar los datos de la solución
    *
    *  @return String con la permutación y el costo de la solución
    */
  override def toString: String = {
    super.toString
    vals + " " + cost

  }

  /** Compara si el costo de esta solución es menor que el de otra
    *
    *  @param other Solución con la que comprar
    *  @return true si el costo es menor que el de other, falso en otro caso
    */
  def <(other: Solution): Boolean ={
    cost < other.cost
  }

  /** Compara si el costo de esta solución es mayor que el de otra
    *
    *  @param other Solución con la que comprar
    *  @return true si el costo es mayor que el de other, falso en otro caso
    */
  def >(other: Solution): Boolean ={
    cost > other.cost
  }
}

