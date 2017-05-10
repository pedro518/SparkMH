package QAP

import org.apache.spark.rdd.RDD

/** El algoritmo Simulated Annealing en distintas versiones.
  * El algorimo base es el del método simulate, utiliza el criterio de aceptación de metrópolis y el esquema de enfriamiento de Cauchy
  * @param instancia Instancia del problema de la que forma parte la solución
  *
  */
class Simulated_Annealing(val instancia:Instance) extends Serializable {

  /** El algoritmo base  de Simulated Annealing en su versión iterativa.
    * Utiliza el criterio de aceptación de metrópolis y el esquema de enfriamiento de Cauchy
    * @param sol_ini Solución desde la que comienza la búsqueda
    *
    */
  def simulate( sol_ini: Solution): Solution = {
    // Parámetros iniciales
    var best = sol_ini
    var actual = sol_ini
    val T_ini = ((0.3 * sol_ini.cost)/(-Math.log(0.3))).toFloat //Temperatura inicial
    var T = T_ini //Temperatura actual
    val TF = 0.001f
    val max_vecinos = 10 * instancia.tam
    val max_exitos = 0.1 * max_vecinos
    var n_vecinos = 0
    var n_exitos = 0
    var it = 0
    var no_exitos = false

    //Bucle Principal
    do{

      //Bucle interior
      while(n_vecinos < max_vecinos && n_exitos < max_exitos){

        val vecino = actual.neightbor(instancia)
        n_vecinos += 1

        //Criterio de aceptación
        if (metrop(vecino.cost-actual.cost, T, it)){
          n_exitos += 1
          actual = vecino


          if(actual < best) {
            best = actual
          }
        }
      } //Fin bucle interior

      if (n_exitos == 0)
        no_exitos = true
      else
        n_exitos = 0

      T *= 0.99f  // Descenso de temperatura Cauchy
      it += 1

      n_vecinos = 0

    }while (!no_exitos && T > TF) //Fin bucle pricipal

    best
  }

  /** Criterio de aceptación de metróplolis.
    * El criterio consiste en aceptar una solución si la diferencia de costos entre una solución y su vecina es negativa
    * o si al generar un número aleatorio entre 0 y 1 este es menor o igual que e**(-de/it*t)
    * @param de Diferencia entre la solución actual y su vecina
    * @param t Temperatura actual
    * @param it Iteración actual
    *
    */
  def metrop(de: Float, t:Float, it: Int): Boolean = {
    de < 0.0 || scala.util.Random.nextFloat() <= Math.exp(-de / (it * t))
  }

  /** Simulated Annealing en una versión paralelizada.
    * Esta versión consiste en dado un conjunto de soluciones iniciales, realizar una búsqueda a cada una de ellas y devolver la mejor de todas.
    * @param sols_ini RDD con el conjunto de soluciones inciales
    * @return La mejor solución encontrada
    *
    */
  def search_MA(sols_ini: RDD[Solution]): Solution ={
    val search: RDD[Solution] = sols_ini.map(x => simulate(x))

     search.sortBy(_.cost).first()
  }

  /** Simulated Annealing en una versión paralelizada.
    * Esta versión consiste en dado un conjunto de soluciones iniciales, realizar una búsqueda a cada una de ellas y quedarse con las 10
    * mejores encontradas, después usar esas mejores encontradas para repetir el proceso, así como tantas repeticiones se desee.
    * @param sols_ini RDD con el conjunto de soluciones inciales
    * @param stop Número de repeticiones deseadas
    * @return La mejor solución encontrada
    *
    */
  def search_MA_REP(sols_ini: RDD[Solution], stop: Int): Solution = {
    //Inicializo valores
    var search: RDD[Solution] = sols_ini
    var mejores: RDD[Solution] = search

    //Comienza el bucle
    for (_ <- 0 until stop) {
        //Realizo las búsquedas
        search = mejores.map(x => simulate(x))

        //Obtengo todas las soluciones ordenadas
        mejores = (search ++ mejores).sortBy(_.cost)

        //Me quedo con las mejores
        val mejores_ok: Array[Solution] = mejores.take(10)
        mejores = mejores.sparkContext.parallelize(mejores_ok)
      }

    mejores.first()

  }
}