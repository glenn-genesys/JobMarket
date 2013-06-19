package JobMarket

import util.Random
import JobSim._

object MarketTesting {;import org.scalaide.worksheet.runtime.library.WorksheetSupport._; def main(args: Array[String])=$execute{;$skip(123); 
  def funPrint[T]( x: T ) = { println(x); x };System.out.println("""funPrint: [T](x: T)T""");$skip(92); 

  def mean[T](xs:Iterable[T])( implicit num: Numeric[T] ) = num.toDouble(xs.sum) / xs.size;System.out.println("""mean: [T](xs: Iterable[T])(implicit num: Numeric[T])Double""");$skip(174); 

  def std[T](xs:Iterable[T])( implicit num: Numeric[T] ) = 	{
    val av = mean(xs)
	  math.sqrt((mean(xs map {v => List(v, v).product}) - av*av)*xs.size/(xs.size - 1))
  };System.out.println("""std: [T](xs: Iterable[T])(implicit num: Numeric[T])Double""");$skip(149); 
  
  def wmean(xs:Iterable[(Double,Double)]) = (xs.foldLeft((0.0, 0.0))((mw, vw) => (mw._1 + vw._1*vw._2, mw._2 + vw._2))) match {case (v,w) => v/w};System.out.println("""wmean: (xs: Iterable[(Double, Double)])Double""");$skip(129); 
 
 
	def normDist( mean: Double, std: Double ) = {
    (((1 to 64) map ( _ => math.random ) sum) - 32.0 )/ 4.0 * std  + mean
  };System.out.println("""normDist: (mean: Double, std: Double)Double""");$skip(24); 

  val numWorkers = 100;System.out.println("""numWorkers  : Int = """ + $show(numWorkers ));$skip(26); 
  val numDisciplines = 10;System.out.println("""numDisciplines  : Int = """ + $show(numDisciplines ));$skip(20); 
  val numJobs = 100;System.out.println("""numJobs  : Int = """ + $show(numJobs ));$skip(20); 
  val jobSize = 0.5;System.out.println("""jobSize  : Double = """ + $show(jobSize ));$skip(145); 
  
  val workers = (1 to numWorkers) map (i => Worker(i.toString, (1 to numDisciplines) map ( _ => 1.0/normRand(1.0, 0.2) abs ) toList )) toList;System.out.println("""workers  : List[JobMarket.Worker] = """ + $show(workers ));$skip(81); 
  
  val allJobs = jobStream( orphanJob(jobSize, jobSize/2.0, numDisciplines ) );System.out.println("""allJobs  : Stream[JobMarket.Job] = """ + $show(allJobs ));$skip(49); 
  val jobs = (allJobs take numJobs force) toList;System.out.println("""jobs  : List[JobMarket.Job] = """ + $show(jobs ));$skip(106); 
  
	val ord1 = for (w <- workers) yield jobs.sortBy( j => (j.workload + j.workerTime(w))/j.workerTime(w));System.out.println("""ord1  : List[List[JobMarket.Job]] = """ + $show(ord1 ));$skip(85); 
	
	val ord2 = for (w <- workers) yield jobs.sortBy( j => j.workload/j.workerTime(w));System.out.println("""ord2  : List[List[JobMarket.Job]] = """ + $show(ord2 ));$skip(20); val res$0 = 
	
	ord1 equals ord2;System.out.println("""res0: Boolean = """ + $show(res$0));$skip(99); 

	val ord3 = for (j <- jobs) yield workers.sortBy( w => j.workload/(j.workload + j.workerTime(w)));System.out.println("""ord3  : List[List[JobMarket.Worker]] = """ + $show(ord3 ));$skip(85); 
	
	val ord4 = for (j <- jobs) yield workers.sortBy( w => j.workload/j.workerTime(w));System.out.println("""ord4  : List[List[JobMarket.Worker]] = """ + $show(ord4 ));$skip(20); val res$1 = 
	
	ord3 equals ord4;System.out.println("""res1: Boolean = """ + $show(res$1));$skip(64); 

  val (ws, fm) = JobSim.marketSim( 3, 0.2, 3, 0.5, 2.0 ).unzip;System.out.println("""ws  : List[List[JobMarket.Worker]] = """ + $show(ws ));System.out.println("""fm  : List[List[JobMarket.Bid]] = """ + $show(fm ));$skip(5); val res$2 = 
  fm;System.out.println("""res2: List[List[JobMarket.Bid]] = """ + $show(res$2));$skip(13); val res$3 = 
  fm.flatten;System.out.println("""res3: List[JobMarket.Bid] = """ + $show(res$3));$skip(37); val res$4 = 
  fm.flatten flatMap {_.worker.bids};System.out.println("""res4: List[JobMarket.Bid] = """ + $show(res$4));$skip(45); val res$5 = 
  (fm.flatten flatMap {_.worker.bids} toSet);System.out.println("""res5: scala.collection.immutable.Set[JobMarket.Bid] = """ + $show(res$5));$skip(50); val res$6 = 
  (fm.flatten flatMap {_.worker.bids} toSet).size;System.out.println("""res6: Int = """ + $show(res$6));$skip(52); val res$7 = 
  (fm flatMap {_ map { b: Bid => b.worker } toSet});System.out.println("""res7: List[JobMarket.Worker] = """ + $show(res$7));$skip(85); val res$8 = 
  (fm flatMap {_ map { b: Bid => b.worker } toSet}) map {w: Worker => w.bids.length};System.out.println("""res8: List[Int] = """ + $show(res$8));$skip(88); val res$9 = 
  (fm flatMap {_ flatMap { b: Bid => Set(b.worker) }}) map {w: Worker => w.bids.length};System.out.println("""res9: List[Int] = """ + $show(res$9));$skip(92); val res$10 = 
  (fm flatMap {_ flatMap { b: Bid => Set(b.worker) }}) map {w: Worker => w.bids.length} sum;System.out.println("""res10: Int = """ + $show(res$10))}

 
  
}