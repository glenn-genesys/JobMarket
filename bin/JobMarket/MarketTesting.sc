package JobMarket

import util.Random
import JobSim._

object MarketTesting {
  def funPrint[T]( x: T ) = { println(x); x }

  def mean[T](xs:Iterable[T])( implicit num: Numeric[T] ) = num.toDouble(xs.sum) / xs.size

  def std[T](xs:Iterable[T])( implicit num: Numeric[T] ) = 	{
    val av = mean(xs)
	  math.sqrt((mean(xs map {v => List(v, v).product}) - av*av)*xs.size/(xs.size - 1))
  }
  
  def wmean(xs:Iterable[(Double,Double)]) = (xs.foldLeft((0.0, 0.0))((mw, vw) => (mw._1 + vw._1*vw._2, mw._2 + vw._2))) match {case (v,w) => v/w}
 
 
	def normDist( mean: Double, std: Double ) = {
    (((1 to 64) map ( _ => math.random ) sum) - 32.0 )/ 4.0 * std  + mean
  }

  val numWorkers = 100
  val numDisciplines = 10
  val numJobs = 100
  val jobSize = 0.5
  
  def histogram[T](xs: Iterable[T], bins: Int)( implicit num: Numeric[T] ) = {
    val (lb, ub) = (xs.min, xs.max)
    val (lx, ux) = (num.toDouble(lb), num.toDouble(ub))
    val rx = bins/ux
    ((xs map { x => ((num.toDouble(x) - lx)*rx) toInt } groupBy(identity)).values.map {vs => (vs.head/rx + lx, vs.size)}).toList.sortBy(_._1)
  }
 5 to 50 by 5 toList
  val rs = (1 to 100) map { b => normDist(10, 2) }
  
  rs.sorted
  
  val h = Histogram(rs, 5)
  (h.size, h map (_._2) sum)
  
  
  1 to 100 map (JobSim.f10ceil(_))
  
  
  (((rs map (_.toInt)) groupBy(identity)).values.map {vs => (vs.head, vs.size)}).toList.sortBy(_._1)
  (((rs sorted) map (_.toInt)) groupBy(identity) values) map (_.size)
  
  val workers = (1 to numWorkers) map (i => Worker(i.toString, (1 to numDisciplines) map ( _ => 1.0/normRand(1.0, 0.2) abs ) toList )) toList
  
  val allJobs = jobStream( orphanJob(jobSize, jobSize/2.0, numDisciplines ) )
  val jobs = (allJobs take numJobs force) toList
  
 val x = List((1,2), (3,4), (5,6), (7,8))
 x map {case (_, r) => r}
 x.unzip._2


  val results = JobSim.comparativeMarketSim( 3, 0.2, 3, 0.5, 2.0, List(CreditMarket,PreferenceMarket) )
 

 
  
}