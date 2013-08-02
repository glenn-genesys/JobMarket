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
  val jobSize = 0.5;System.out.println("""jobSize  : Double = """ + $show(jobSize ));$skip(341); 
  
  def histogram[T](xs: Iterable[T], bins: Int)( implicit num: Numeric[T] ) = {
    val (lb, ub) = (xs.min, xs.max)
    val (lx, ux) = (num.toDouble(lb), num.toDouble(ub))
    val rx = bins/ux
    ((xs map { x => ((num.toDouble(x) - lx)*rx) toInt } groupBy(identity)).values.map {vs => (vs.head/rx + lx, vs.size)}).toList.sortBy(_._1)
  };System.out.println("""histogram: [T](xs: Iterable[T], bins: Int)(implicit num: Numeric[T])List[(Double, Int)]""");$skip(21); val res$0 = 
 5 to 50 by 5 toList;System.out.println("""res0: List[Int] = """ + $show(res$0));$skip(51); 
  val rs = (1 to 100) map { b => normDist(10, 2) };System.out.println("""rs  : scala.collection.immutable.IndexedSeq[Double] = """ + $show(rs ));$skip(15); val res$1 = 
  
  rs.sorted;System.out.println("""res1: scala.collection.immutable.IndexedSeq[Double] = """ + $show(res$1));$skip(40); 
  
  val h = Histogram(rs, minBins = 5);System.out.println("""h  : JobMarket.Histogram = """ + $show(h ));$skip(26); val res$2 = 
  (h.numBins, h.data sum);System.out.println("""res2: (Int, Double) = """ + $show(res$2));$skip(44); val res$3 = 
  
  
  1 to 100 map (Histogram.f10ceil(_));System.out.println("""res3: scala.collection.immutable.IndexedSeq[Double] = """ + $show(res$3));$skip(107); val res$4 = 
  
  
  (((rs map (_.toInt)) groupBy(identity)).values.map {vs => (vs.head, vs.size)}).toList.sortBy(_._1);System.out.println("""res4: List[(Int, Int)] = """ + $show(res$4));$skip(70); val res$5 = 
  (((rs sorted) map (_.toInt)) groupBy(identity) values) map (_.size);System.out.println("""res5: Iterable[Int] = """ + $show(res$5));$skip(145); 
  
  val workers = (1 to numWorkers) map (i => Worker(i.toString, (1 to numDisciplines) map ( _ => 1.0/normRand(1.0, 0.2) abs ) toList )) toList;System.out.println("""workers  : List[JobMarket.Worker] = """ + $show(workers ));$skip(81); 
  
  val allJobs = jobStream( orphanJob(jobSize, jobSize/2.0, numDisciplines ) );System.out.println("""allJobs  : Stream[JobMarket.Job] = """ + $show(allJobs ));$skip(49); 
  val jobs = (allJobs take numJobs force) toList;System.out.println("""jobs  : List[JobMarket.Job] = """ + $show(jobs ));$skip(45); 
  
 val x = List((1,2), (3,4), (5,6), (7,8));System.out.println("""x  : List[(Int, Int)] = """ + $show(x ));$skip(26); val res$6 = 
 x map {case (_, r) => r};System.out.println("""res6: List[Int] = """ + $show(res$6));$skip(12); val res$7 = 
 x.unzip._2;System.out.println("""res7: List[Int] = """ + $show(res$7));$skip(23); val res$8 = 


 1 to numDisciplines;System.out.println("""res8: scala.collection.immutable.Range.Inclusive = """ + $show(res$8));$skip(12); val res$9 = 
 workers(0);System.out.println("""res9: JobMarket.Worker = """ + $show(res$9));$skip(57); val res$10 = 
 
 0 until numDisciplines map (workers(0).efficiency(_));System.out.println("""res10: scala.collection.immutable.IndexedSeq[Double] = """ + $show(res$10))}
}