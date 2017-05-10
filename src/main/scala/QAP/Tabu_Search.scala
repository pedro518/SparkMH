package QAP

import org.apache.spark.rdd.RDD

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

/** El algoritmo Tabú Search en distintas versiones.
  * El algorimo base es el del método search
  *
  * @param instancia Instancia del problema de la que forma parte la solución
  *
  */
class Tabu_Search(val instancia:Instance) extends Serializable {

  def search( sol_ini: Solution): Solution = {
    //Parámetros iniciales
    var actual: Solution = sol_ini
    var best:Solution = sol_ini
    var vecino:Solution = sol_ini
    val lista_tabu: mutable.Queue[Solution] = new mutable.Queue[Solution]
    var soluciones_elite: ListBuffer[Solution] = new ListBuffer[Solution]
    var estabu:Boolean = true
    var supCrit:Boolean = true
    var indiceDeMejorVecinoValido:Int = 0

    ////////////////////////////////////////

    //Parámtros configurables de la búsqueda
    val max_vecinos:Int = 30
    val max_iteracciones:Int = 25000
    val tamtabu:Int = instancia.tam*2
    val n_mejores: Int = 30

    //Bucle principal
    for(_ <- 0 until max_iteracciones){
      //Genero max_vecinos
      var vecinos: List[Solution] = actual.neightborhood(instancia, max_vecinos)
      //Los ordenos de mejor a peor y me quedo con el mejor que no sea tabú o, si lo es, que supere el criterio de aspiracion

      vecinos = vecinos.sortBy(_.cost)

      estabu= true
      supCrit = false
      indiceDeMejorVecinoValido = 0

      while(estabu && !supCrit) {
        vecino = vecinos(indiceDeMejorVecinoValido)

        estabu = isTabu(lista_tabu, vecino)

        supCrit = vecino < best


        if (estabu)
          indiceDeMejorVecinoValido += 1

        if (indiceDeMejorVecinoValido >= max_vecinos) {
          actual = reinicializartabu(lista_tabu, soluciones_elite)
        }
      }


      actual = vecino
        //Si el vecino escogido supera el criterio de aspiracion, actualizo el mejor encontrado
        if(supCrit){

          best = actual
          //Añado la soluciona a las soluciones élite, si es posible
          if(soluciones_elite.length > n_mejores){
            soluciones_elite.sortBy(_.cost)
            if(soluciones_elite.tail.head > best) {
              soluciones_elite = soluciones_elite.dropRight(1)
              soluciones_elite.append(best)
            }
          }else soluciones_elite.append(best)

        }

        //Introduzco la solución en la lista
        if(lista_tabu.length > tamtabu) lista_tabu.dequeue()
        lista_tabu.enqueue(actual)

    }

    best
  }

  /** Comprueba si una solución está en la lista tabú.
    *
    * @param lista Lista con las soluciones tabú
    * @param sol Solución a comprobar si es tabú
    * @return true si la solución es tabú, false en otro caso
    *
    */
  def isTabu(lista: mutable.Queue[Solution], sol: Solution): Boolean = lista.contains(sol)

  /** Reinicializa la búsqueda
    * Devuelve una solución desde la que volver a empezar la búsqueda, esta puede ser una solución aleatoria o un de las
    * soluciones élite que se han encontrado hasta ahora
    *
    * @param lista_tabu Lista con las soluciones tabú
    * @param soluciones_elite Lista de las soluciones élite encontradas hasta el momento
    * @return La solución desde la que volver a empezar
    *
    */
  def reinicializartabu(lista_tabu: mutable.Queue[Solution], soluciones_elite:ListBuffer[Solution]): Solution = {
    val tamtabu = lista_tabu.length
    val aux = lista_tabu.head
    lista_tabu.clear()

    for(_ <- 0 until tamtabu)
      lista_tabu.enqueue(aux)

    if(scala.util.Random.nextFloat() < 0.5f)
      soluciones_elite(scala.util.Random.nextInt(soluciones_elite.length))
    else
      instancia.Random_sol()
  }

  def search_MTS(sols_ini: RDD[Solution]): Solution ={
    val search: RDD[Solution] = sols_ini.map(x => this.search(x))

    search.max()
  }

  def search_MTS_REP(sols_ini: RDD[Solution], stop: Int): Solution = {
    //Inicializo valores
    var search: RDD[Solution] = sols_ini

    //Comienza el bucle
    for (_ <- 0 until stop) {
      //Realizo las búsquedas
      search = search.map(x => this.search(x)).sortBy(_.cost)
    }

    search.first()

  }

}
