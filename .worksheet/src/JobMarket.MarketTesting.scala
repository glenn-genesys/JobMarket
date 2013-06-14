package JobMarket

import util.Random

object MarketTesting {;import org.scalaide.worksheet.runtime.library.WorksheetSupport._; def main(args: Array[String])=$execute{;$skip(107); 
  def funPrint[T]( x: T ) = { println(x); x };System.out.println("""funPrint: [T](x: T)T""");$skip(92); 

  def mean[T](xs:Iterable[T])( implicit num: Numeric[T] ) = num.toDouble(xs.sum) / xs.size;System.out.println("""mean: [T](xs: Iterable[T])(implicit num: Numeric[T])Double""");$skip(174); 

  def std[T](xs:Iterable[T])( implicit num: Numeric[T] ) = 	{
    val av = mean(xs)
	  math.sqrt((mean(xs map {v => List(v, v).product}) - av*av)*xs.size/(xs.size - 1))
  };System.out.println("""std: [T](xs: Iterable[T])(implicit num: Numeric[T])Double""");$skip(149); 
  
  def wmean(xs:Iterable[(Double,Double)]) = (xs.foldLeft((0.0, 0.0))((mw, vw) => (mw._1 + vw._1*vw._2, mw._2 + vw._2))) match {case (v,w) => v/w};System.out.println("""wmean: (xs: Iterable[(Double, Double)])Double""");$skip(90); 

  // def mean(xs: List[Double]) = xs.sum / xs.size

	val (minWork, maxWork) = (0.2, 1.0);System.out.println("""minWork  : Double = """ + $show(minWork ));System.out.println("""maxWork  : Double = """ + $show(maxWork ));$skip(1191); 

	  def marketBidding( ws: List[Worker], js: List[Job], offers: Map[Job, (Worker, Double)] ): Map[Job, (Worker, Double)] = {
	      // Get bids of given workers for given jobs
		  val bids = Market.workerBids(ws, js, offers)
	
	      // Update current best offers
		  val current = Market.considerOffers( bids, { case (j, (w, p)) => j.workerTime(w)/p }, offers )
		  
		  // Bidding stops when no new bids are accepted in a round
		  if (current equals offers) return current
		  
		  val tmap = Market.timeCommitment(current)
		  
		  // Determine the average crediting rate implied by current allocation
		  val cmap = Market.creditRate(current)
		   
		  // Update crediting rate of workers according to whether their bid was rejected or not
		  val updatedWorkers = ws map { w => Worker(w.name, w.efficiency, cmap.getOrElse(w, w.rate * 0.97), tmap.getOrElse(w, 0.0) + w.committed,
		    										(bids collect { case (`w`, (j, b)) => (j, b) }):::w.bids ) }
		  
		  // Partition into those workers that need to rebid, and those that can sit
		  val (sitters, rebidders) = updatedWorkers partition {_.committed > minWork}
	
			current
		  // marketBidding( rebidders, js, current )
	  };System.out.println("""marketBidding: (ws: List[JobMarket.Worker], js: List[JobMarket.Job], offers: Map[JobMarket.Job,(JobMarket.Worker, Double)])Map[JobMarket.Job,(JobMarket.Worker, Double)]""");$skip(129); 
	  
	def normDist( mean: Double, std: Double ) = {
    (((1 to 64) map ( _ => math.random ) sum) - 32.0 )/ 4.0 * std  + mean
  };System.out.println("""normDist: (mean: Double, std: Double)Double""");$skip(28); 
  
  val numDisciplines = 5;System.out.println("""numDisciplines  : Int = """ + $show(numDisciplines ));$skip(20); 
	val numWorkers = 5;System.out.println("""numWorkers  : Int = """ + $show(numWorkers ));$skip(20); 
	
	val numJobs = 10;System.out.println("""numJobs  : Int = """ + $show(numJobs ));$skip(141); 
	  
	val workers = (1 to numWorkers) map (i => Worker(i.toString, (1 to numDisciplines) map ( _ => normDist(1.0, 0.5) abs ) toList )) toList;System.out.println("""workers  : List[JobMarket.Worker] = """ + $show(workers ));$skip(75); 
 
	val jobs = JobSim.orphanJobs( numJobs, numWorkers, 0.3, numDisciplines);System.out.println("""jobs  : List[JobMarket.Job] = """ + $show(jobs ));$skip(23); 

  println("Workers:");$skip(61); 
  for (w <- workers) println( w.name + ": " + w.efficiency );$skip(22); 
  
  println("Jobs:");$skip(52); 
  for (j <- jobs) println( j.id + ": " + j.skills );$skip(59); 

	val jobPrefs = workers.map( w => (w, w.jobPrefs(jobs)) );System.out.println("""jobPrefs  : List[(JobMarket.Worker, List[JobMarket.Job])] = """ + $show(jobPrefs ));$skip(88); 
	  
	val jobValues = jobPrefs map { case (w, j::_) => j -> j.workload/j.workerTime(w) };System.out.println("""jobValues  : List[(JobMarket.Job, Double)] = """ + $show(jobValues ));$skip(172); 
	  
  val allBids = workers.map( w => (w, jobs map {j => (j, (w.bids.collectFirst { case (`j`, b) => b*0.95 }).getOrElse((j.workload + j.workerTime(w)*w.rate)/2.0) ) } ) );System.out.println("""allBids  : List[(JobMarket.Worker, List[(JobMarket.Job, Double)])] = """ + $show(allBids ));$skip(91); val res$0 = 
  
  allBids map { case (w, jbs) => (w, jbs maxBy { case (j, b) => b/j.workerTime(w) } ) };System.out.println("""res0: List[(JobMarket.Worker, (JobMarket.Job, Double))] = """ + $show(res$0));$skip(38); 
 
	val m = new Market(numDisciplines);System.out.println("""m  : JobMarket.Market = """ + $show(m ));$skip(49); 

	val mc = m.deferredAcceptance( workers, jobs );System.out.println("""mc  : scala.collection.immutable.Map[JobMarket.Job,JobMarket.Worker] = """ + $show(mc ));$skip(70); val res$1 = 
   
  marketBidding( workers, jobs, Map.empty[Job, (Worker, Double)]);System.out.println("""res1: Map[JobMarket.Job,(JobMarket.Worker, Double)] = """ + $show(res$1));$skip(57); 
  
	val (mb, ww) = m.marketBidding( workers, jobs, 0.2 );System.out.println("""mb  : scala.collection.immutable.Map[JobMarket.Job,(JobMarket.Worker, Double)] = """ + $show(mb ));System.out.println("""ww  : List[JobMarket.Worker] = """ + $show(ww ));$skip(49); val res$2 = 
	
	(jobs.toSet &~ mb.keySet).toList.sortBy(_.id);System.out.println("""res2: List[JobMarket.Job] = """ + $show(res$2));$skip(33); val res$3 = 
	(mb groupBy { _._2._1 } keySet);System.out.println("""res3: scala.collection.immutable.Set[JobMarket.Worker] = """ + $show(res$3));$skip(50); val res$4 = 
	workers.toSet &~ (mb groupBy { _._2._1 } keySet);System.out.println("""res4: scala.collection.immutable.Set[JobMarket.Worker] = """ + $show(res$4))}
}