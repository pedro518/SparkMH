package QAP

import org.apache.spark.rdd.RDD


class Simulated_Annealing(val instancia:Instance) extends Serializable {

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
    val bests = sols_ini
    var search = sols_ini

    var fin = bests.take(1)

    for (_ <- 0 until stop) {
        search = search.map(x => simulate(x))

        val mejores = search ++ bests
        val mejores_ord = mejores.collect().sortWith((a,b) => a < b)
        val mejores_rdd = mejores.sparkContext.parallelize(mejores_ord)
        fin = mejores_rdd.take(10)
      }

    fin(0)

  }
}