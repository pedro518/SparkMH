import QAP.{Instance, Simulated_Annealing, Solution}
import org.apache.spark.SparkContext
import org.apache.spark.SparkConf
import org.apache.spark.rdd.RDD



/**
  * Created by Pedro on 30/03/2017.
  */

object Main {
  def main(args: Array[String]): Unit = {
    var instancia = new Instance(args(0))
    val simulated_Annealing = new Simulated_Annealing(instancia)


    val sc = new SparkContext(new SparkConf().setMaster("local").setAppName("Spark Annealing"))
    val sols_ale: RDD[Solution] = sc.parallelize(instancia.Random_sol(args(1).toInt))

    var now = System.nanoTime
    val search= simulated_Annealing.simulate(instancia.Random_sol())
    var elapsed1 = (System.nanoTime - now).toFloat/1000000000f
    now = System.nanoTime
    val search_MA= simulated_Annealing.search_MA(sols_ale)
    var elapsed2 = (System.nanoTime - now).toFloat/1000000000f
    now = System.nanoTime
    val search_MA_REP= simulated_Annealing.search_MA_REP(sols_ale,10)
    var elapsed3 = (System.nanoTime - now).toFloat/1000000000f


    sc.stop()
    println("Simple Simulate:")
    println(search)
    println("Tiempo: " + elapsed1)

    println("MA Simulate:")
    println(search_MA)
    println("Tiempo: " + elapsed2)

    println("MA REP Simulate:")
    println(search_MA_REP)
    println("Tiempo: " + elapsed3)

    //sc.parallelize(aux).saveAsTextFile("Salida")

  }
}