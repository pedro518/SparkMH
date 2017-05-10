import QAP._
import org.apache.spark.SparkContext
import org.apache.spark.SparkConf
import org.apache.spark.rdd.RDD

/**
  * Created by Pedro on 30/03/2017.
  */

object Main {
  def main(args: Array[String]): Unit = {

    if (args.length != 2){
      println("Uso: JAR <instancia> <numero_soluciones_iniciales>")

      sys.exit()
    }

    val instancia = new Instance(args(0))


    val tabu_search = new Tabu_Search(instancia)

    val sc = new SparkContext(new SparkConf().setMaster("local").setAppName("Spark Annealing"))
    val sols_ale: RDD[Solution] = sc.parallelize(instancia.Random_sol(args(1).toInt))

    var now = System.nanoTime
    val search= tabu_search.search(instancia.Random_sol())
    val elapsed1 = (System.nanoTime - now).toFloat / 1000000000f
    now = System.nanoTime
    val search_MTS= tabu_search.search_MTS(sols_ale)
    val elapsed2 = (System.nanoTime - now).toFloat / 1000000000f
    now = System.nanoTime
    val search_MTS_REP= tabu_search.search_MTS_REP(sols_ale,10)
    val elapsed3 = (System.nanoTime - now).toFloat/1000000000f


    sc.stop()
    println("Simple Tabú Search:")
    println(search)
    println("Tiempo: " + elapsed1 + " segundos")

    println("Multi Tabú Search:")
    println(search_MTS)
    println("Tiempo: " + elapsed2 + " segundos")

    println("Multi REP Tabú Search:")
    println(search_MTS_REP)
    println("Tiempo: " + elapsed3 + " segundos")

  }
}