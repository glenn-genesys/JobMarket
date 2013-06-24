package JobMarket

import util.Random
import JobSim._

object MarketTesting {
  def funPrint[T]( x: T ) = { println(x); x }     //> funPrint: [T](x: T)T

  def mean[T](xs:Iterable[T])( implicit num: Numeric[T] ) = num.toDouble(xs.sum) / xs.size
                                                  //> mean: [T](xs: Iterable[T])(implicit num: Numeric[T])Double

  def std[T](xs:Iterable[T])( implicit num: Numeric[T] ) = 	{
    val av = mean(xs)
	  math.sqrt((mean(xs map {v => List(v, v).product}) - av*av)*xs.size/(xs.size - 1))
  }                                               //> std: [T](xs: Iterable[T])(implicit num: Numeric[T])Double
  
  def wmean(xs:Iterable[(Double,Double)]) = (xs.foldLeft((0.0, 0.0))((mw, vw) => (mw._1 + vw._1*vw._2, mw._2 + vw._2))) match {case (v,w) => v/w}
                                                  //> wmean: (xs: Iterable[(Double, Double)])Double
 
 
	def normDist( mean: Double, std: Double ) = {
    (((1 to 64) map ( _ => math.random ) sum) - 32.0 )/ 4.0 * std  + mean
  }                                               //> normDist: (mean: Double, std: Double)Double

  val numWorkers = 100                            //> numWorkers  : Int = 100
  val numDisciplines = 10                         //> numDisciplines  : Int = 10
  val numJobs = 100                               //> numJobs  : Int = 100
  val jobSize = 0.5                               //> jobSize  : Double = 0.5
  
  0 until 1                                       //> res0: scala.collection.immutable.Range = Range(0)
  
  val workers = (1 to numWorkers) map (i => Worker(i.toString, (1 to numDisciplines) map ( _ => 1.0/normRand(1.0, 0.2) abs ) toList )) toList
                                                  //> workers  : List[JobMarket.Worker] = List(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 
                                                  //| 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 
                                                  //| 31, 32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 
                                                  //| 50, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60, 61, 62, 63, 64, 65, 66, 67, 68, 
                                                  //| 69, 70, 71, 72, 73, 74, 75, 76, 77, 78, 79, 80, 81, 82, 83, 84, 85, 86, 87, 
                                                  //| 88, 89, 90, 91, 92, 93, 94, 95, 96, 97, 98, 99, 100)
  
  val allJobs = jobStream( orphanJob(jobSize, jobSize/2.0, numDisciplines ) )
                                                  //> allJobs  : Stream[JobMarket.Job] = Stream(0, ?)
  val jobs = (allJobs take numJobs force) toList  //> jobs  : List[JobMarket.Job] = List(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12
                                                  //| , 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 3
                                                  //| 1, 32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 
                                                  //| 50, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60, 61, 62, 63, 64, 65, 66, 67, 68,
                                                  //|  69, 70, 71, 72, 73, 74, 75, 76, 77, 78, 79, 80, 81, 82, 83, 84, 85, 86, 87
                                                  //| , 88, 89, 90, 91, 92, 93, 94, 95, 96, 97, 98, 99)
  
 val x = List((1,2), (3,4), (5,6), (7,8))         //> x  : List[(Int, Int)] = List((1,2), (3,4), (5,6), (7,8))
 x map {case (_, r) => r}                         //> res1: List[Int] = List(2, 4, 6, 8)
 x.unzip._2                                       //> res2: List[Int] = List(2, 4, 6, 8)


  val results = JobSim.comparativeMarketSim( 3, 0.2, 3, 0.5, 2.0, List(CreditMarket,PreferenceMarket) )
                                                  //> Workers:
                                                  //| 1: List(0.7294432142380488, 0.9185956162576969, 1.3321739581431356)
                                                  //| 2: List(0.9516143303554515, 0.6704645258335539, 1.1293963147505601)
                                                  //| 3: List(0.9176239419912169, 1.1066408572319566, 1.0026132577116382)
                                                  //| Jobs: 1.9430719315265004
                                                  //| 0: List(0.09631750381903201, 0.1270723749531877, 0.05879464745843844)
                                                  //| 1: List(0.03430962094964917, 0.062071313110057424, 0.0812925444948755)
                                                  //| 2: List(0.0176041567825061, 0.004383906631869956, 0.05505046167399329)
                                                  //| 3: List(0.024585576174064268, 0.005504402462141328, 0.008229350375965623)
                                                  //| 4: List(0.05203250304544407, 0.026601642909027907, 0.10600725501983031)
                                                  //| 5: List(0.017385251698124105, 0.03501801731205223, 0.09301683231235724)
                                                  //| 6: List(0.0018154606329252418, 0.0031504379168918076, 0.1732945559223308)
                                                  //| 7: List(0.0542566229014599, 0.07300094008304404, 0.1824913972235274)
                                                  //| 8: List(0.01789368381602027, 0.2553782385636003, 0.008364295662579322)
                                                  //| 9: List(0.05604548768514032, 0.157378563770
                                                  //| Output exceeds cutoff limit.
 

 
  
}