package QAP

import org.apache.spark.rdd.RDD

import scala.util.control._

class Simulated_Annealing(val instancia:Instance) extends Serializable {
  
  def search( sol_ini: Solution, t_inicial: Float): Solution = {
    //Paramámetros configurables
    val max_vecinos: Int = 100 * instancia.tam
    val max_exitos: Int = (0.5 * max_vecinos).toInt
    val t_final: Double = 0.1
    val max_iteraciones: Int = 25000
    val n_enfriamientos: Int = max_iteraciones/instancia.tam

  
    
    ////////////////////////////////////////////////    
    
    var actual: Solution = sol_ini
    var best: Solution = actual
    var vecino: Solution = actual
    var intentos: Int=0

    val TFACTR: Float = ((t_inicial - t_final)/(n_enfriamientos * t_inicial * t_final)).toFloat
    
    var n_exitos = 0
    var t: Float = t_inicial
    
    val loop1 = new Breaks
    loop1.breakable {
      for (j <- 0 to max_iteraciones){
       //Inicializamos el contador de exitos a 0 para la temperatura actual.
       n_exitos = 0
  
       /* Bucle interno: generacion de vecinos para la temperatura actual.
          Finaliza cuando se hayan generado max_vecinos o cuando se hayan
          contabilizado max_exitos. */
       val loop2 = new Breaks
       loop2.breakable {
         for (k <- 0 to max_vecinos){
           /* Obtencion de un vecino de la solucion actual */
           vecino = actual.neightbor(instancia)
    
           /* En el caso en que el nuevo vecino es aceptado: */

           if (metrop(vecino.cost - actual.cost,t, 8))
            {
             /* Contamos un nuevo exito */
             n_exitos += 1
    
             /* Actualizamos la solucion actual */
             actual = vecino
    
             /* Actualizamos la mejor solucion hallada hasta el momento */
             if (actual < best) {
               best = actual
             }

             }
    
           /* Saltamos a la siguiente iteracion (es decir, enfriamos la temperatura)
              si ya hemos sobrepasado el numero de exitos especificado */
           if (n_exitos == max_exitos) loop2.break
         }
       }
  
       /* Enfriamiento proporcional de la temperatura */
       t *= TFACTR;
  
       /* Terminamos la ejecucion del algoritmo en el caso en que no se consiga
          ningun exito para una temperatura concreta */
       if (n_exitos == 0)loop1.break()
       }
    }

    //Devuelvo la mejor solución
    best   
  
  }

  def simulate( sol_ini: Solution): Solution = {
    // Parámetros iniciales
    var best = sol_ini
    var actual = sol_ini
    val T_ini = ((0.3 * sol_ini.cost)/(-Math.log(0.3))).toFloat //Temperatura inicial
    var T = T_ini //Temperatura actual
    val TF = 0.001f
    val max_vecinos = 10 * instancia.tam;
    val max_exitos = 0.1 * max_vecinos;
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
  
  def metrop(de: Float, t:Float, it: Int): Boolean = {
    de < 0.0 || scala.util.Random.nextFloat() <= Math.exp(-de / (it * t))
  }

  def search_MA(sols_ini: RDD[Solution]): Solution ={
    val search: RDD[Solution] = sols_ini.map(x => this.simulate(x))

    val array_sol = search.collect()

    val aux = array_sol.sortWith((a,b) => a < b)

    aux(0)
  }


  def search_MA_REP(sols_ini: RDD[Solution], stop: Int): Solution = {
    var bests = sols_ini
    var search = sols_ini
    var other = search.filter(x => x < bests.collect().sortWith((a,b) => a < b)(0))
    var other2 = bests.filter(x => x < search.collect().sortWith((a,b) => a < b)(0))
    var mejor: Solution = sols_ini.collect()(0)
    var fin = bests.take(1)
    for (i <- 0 to stop - 1) {
        search = search.map(x => simulate(x))

        val mejores = search ++ bests
        val mejores_ord = mejores.collect().sortWith((a,b) => a < b)
        val mejores_rdd = mejores.sparkContext.parallelize(mejores_ord)
        fin = mejores_rdd.take(10)
      }

    fin(0)

  }
}