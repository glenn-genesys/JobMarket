package JobMarket

import util.Random

object MarketTesting {;import org.scalaide.worksheet.runtime.library.WorksheetSupport._; def main(args: Array[String])=$execute{;$skip(107); 
  def funPrint[T]( x: T ) = { println(x); x };System.out.println("""funPrint: [T](x: T)T""");$skip(92); 

  def mean[T](xs:Iterable[T])( implicit num: Numeric[T] ) = num.toDouble(xs.sum) / xs.size;System.out.println("""mean: [T](xs: Iterable[T])(implicit num: Numeric[T])Double""");$skip(174); 

  def std[T](xs:Iterable[T])( implicit num: Numeric[T] ) = 	{
    val av = mean(xs)
	  math.sqrt((mean(xs map {v => List(v, v).product}) - av*av)*xs.size/(xs.size - 1))
  };System.out.println("""std: [T](xs: Iterable[T])(implicit num: Numeric[T])Double""");$skip(149); 
  
  def wmean(xs:Iterable[(Double,Double)]) = (xs.foldLeft((0.0, 0.0))((mw, vw) => (mw._1 + vw._1*vw._2, mw._2 + vw._2))) match {case (v,w) => v/w};System.out.println("""wmean: (xs: Iterable[(Double, Double)])Double""");$skip(129); 
 
 
	def normDist( mean: Double, std: Double ) = {
    (((1 to 64) map ( _ => math.random ) sum) - 32.0 )/ 4.0 * std  + mean
  };System.out.println("""normDist: (mean: Double, std: Double)Double""");$skip(66); 
  
  val (ws, fm) = JobSim.marketSim( 3, 0.2, 3, 0.5, 2.0 ).unzip;System.out.println("""ws  : List[List[JobMarket.Worker]] = """ + $show(ws ));System.out.println("""fm  : List[List[JobMarket.Bid]] = """ + $show(fm ));$skip(5); val res$0 = 
  fm;System.out.println("""res0: List[List[JobMarket.Bid]] = """ + $show(res$0));$skip(13); val res$1 = 
  fm.flatten;System.out.println("""res1: List[JobMarket.Bid] = """ + $show(res$1));$skip(37); val res$2 = 
  fm.flatten flatMap {_.worker.bids};System.out.println("""res2: List[JobMarket.Bid] = """ + $show(res$2));$skip(45); val res$3 = 
  (fm.flatten flatMap {_.worker.bids} toSet);System.out.println("""res3: scala.collection.immutable.Set[JobMarket.Bid] = """ + $show(res$3));$skip(50); val res$4 = 
  (fm.flatten flatMap {_.worker.bids} toSet).size;System.out.println("""res4: Int = """ + $show(res$4));$skip(52); val res$5 = 
  (fm flatMap {_ map { b: Bid => b.worker } toSet});System.out.println("""res5: List[JobMarket.Worker] = """ + $show(res$5));$skip(85); val res$6 = 
  (fm flatMap {_ map { b: Bid => b.worker } toSet}) map {w: Worker => w.bids.length};System.out.println("""res6: List[Int] = """ + $show(res$6));$skip(88); val res$7 = 
  (fm flatMap {_ flatMap { b: Bid => Set(b.worker) }}) map {w: Worker => w.bids.length};System.out.println("""res7: List[Int] = """ + $show(res$7));$skip(92); val res$8 = 
  (fm flatMap {_ flatMap { b: Bid => Set(b.worker) }}) map {w: Worker => w.bids.length} sum;System.out.println("""res8: Int = """ + $show(res$8))}

 
  
}