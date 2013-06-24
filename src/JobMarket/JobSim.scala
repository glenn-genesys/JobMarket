package JobMarket

import util.Random
import scala.collection.immutable.Map
import scala.collection.mutable.LinkedHashMap
import scala.collection.GenTraversable

/**
 * @author burgessg
 * 
 * The Job market simulation application object
 * Manages the simulation
 *
 */
object JobSim extends App {
  
  def funPrint[T]( x: T ) = { println(x); x }   
  def mean[T](xs:Iterable[T])( implicit num: Numeric[T] ) = num.toDouble(xs.sum) / xs.size
  def std[T](xs:Iterable[T])( implicit num: Numeric[T] ) = 	{
    val av = mean(xs)
	math.sqrt((mean(xs map {v => List(v, v).product}) - av*av)*xs.size/(xs.size - 1))
  }                                               
  
  /** Calculate weighted mean value of a list of tuples (value, weight) */
  // def wmean(xs:Iterable[(Double,Double)]): Double = (xs.foldLeft((0.0, 0.0)) { case ((vs, ws), (v, w)) => (vs + v*w, ws + w) } ) match {case (vs,ws) => vs/ws}
  
  def wmean(xs:Iterable[Estimate]): Double = (xs.foldLeft((0.0, 0.0)) { case ((vs, ws), est) => (vs + est.v*est.w, ws + est.w) } ) match {case (vs,ws) => vs/ws}
  // def wmeano(xs:Iterable[(Double,Option[Double])]): Double = 
  //   (xs.foldLeft((0.0, 0.0)) { case ((vs, ws), (v, w)) => (vs + v*w.getOrElse(0.0), ws + w.getOrElse(0.0)) } ) match {case (vs,ws) => vs/ws}

  /** Calculate the weighted mean and estimate the population standard deviation from a list of tuples (value, weight)
   *  Note that weights can be estimated from std using: w = 1/std^2
   */
  // def wmeanstd( wvs: Iterable[(Double, Double)] ) = (wmean(wvs), std(wvs map {_._1}))
  
  def wmeanstd( wvs: Iterable[Estimate] ) = Estimate(wmean(wvs), std(wvs map {_.v}))
  
  // def wmeanstdo( wvs: Iterable[(Double, Option[Double])] ) = (wmeano(wvs), std(wvs map {_._1}))
  
  /* 
  •	Define k disciplines
  •	Generate n x k efficiency matrix: efficiency of each worker in each discipline
  •	Create n jobs with single disciplines, fixed size and duration
  •	Treat each round as a matching market – every worker to every job
	o	Ie. use worker-proposing, deferred acceptance algorithm to find a stable matching
  •	More generally, market has a list of jobs and a list of workers which it matches each round
*/ 
  
  /**
   * Generate an approximately normally distributed random number with given mean and standard deviation
   */
  def normRand( mean: Double, std: Double ) = {
    // (((1 to 64) map ( _ => math.random ) sum) - 32.0 )/ 4.0 * std  + mean
    Random.nextGaussian * std + mean
  }
  
  /**
   * Return a randomised list of n coefficients that add up to total, 
   * with a highly-skewed pseudo-exponential distribution
   */
  def expExtraction( total: Double, n: Int ): List[Double] = expExtraction0(total, n, Nil)
  
  def expExtraction0( remain: Double, n: Int, acc: List[Double] ): List[Double] = n match {
  	case 1 => Random.shuffle(remain::acc)
  	case _ => {
  		val x = math.random*remain
  				expExtraction0( remain-x, n-1, x::acc )
  	}
  } 

  /** Generate a neverending, lazily-evaluated sequence of Jobs using the given generator */
  def jobStream(gen: Int => Job): Stream[Job] = {
    def jobStream(i: Int, gen: Int => Job): Stream[Job] = gen(i) #:: jobStream(i+1, gen)
    
    jobStream(0, gen)
  }
  
  /**
   * Generate a number of singleton jobs (ie. no parent)
   * Each job is a mixture of skill areas, with the work in each exponentially distributed.
   * @param numJobs Number of jobs to generate
   * @param totalWork Total amount of work (person-years) that the jobs should (approximately) add up to
   * @param std The standard deviation of work in each job
   * @param numDisciplines The number of discipline or skill areas the jobs will be distributed over.
   * @return List of jobs
   */
  def orphanJob( avWork: Double, std: Double, numDisciplines: Int ): Int => Job = Job(_, None, expExtraction( normRand(avWork, std).abs, numDisciplines ))
  
  def orphanJobs( avWork: Double, std: Double, numDisciplines: Int ) = jobStream( orphanJob(avWork, std, numDisciplines ) )

  def orphanJobs( numJobs: Int, totalWork: Double, std: Double, numDisciplines: Int ) = jobStream( orphanJob(totalWork/numJobs, std, numDisciplines ) ) take(numJobs) toList 
  
  def printStats( matching: Map[Job, Worker] ) {
	  println("1 to n matching= " + matching)
	  println("1 to n commitment= " + Market.commitment(matching))

	  // println("Number of unmatched jobs: " + (numJobs - matching.size) + "/" + numJobs)
	  println("Amount of allocated work: " + (matching.keySet map {_.workload} sum) )
	  println("Amount of allocated time: " + (matching map { case (j, w) => j.workerTime(w) } sum) )

	  println("Worker average value: " + (matching groupBy { _._2 } map { case (ww, jws) => (ww, wmean( jws map { case (j, w) => Estimate.byWeight(j.workload/j.workerTime(w), j.workerTime(w)) } )) } ))
	  println("Job value: " + (matching map { case (j, w) => j.workload/j.workerTime(w) }))
	  println("Average value: " + wmean( matching map { case (j, w) => Estimate.byWeight(j.workload/j.workerTime(w), j.workerTime(w)) } ))
  }

  def printBidStats( matching: List[Bid] ) {
	  val totalWork = matching map {_.workload} sum
	  val timeWorked = matching map { _.timeload } sum
	  val numWorkers = (matching map {_.worker} toSet).size

      println("1 to n matching= " + matching)
	  println("1 to n commitment= " + Market.timeCommitment(matching))

	  // println("Number of unmatched jobs: " + (numJobs - matching.size) + "/" + numJobs)
	  println("Amount of allocated work: " + totalWork + " = " + totalWork/numWorkers + " per worker")
	  println("Amount of allocated time: " + timeWorked + " = " + timeWorked/numWorkers + " per worker" )

	  println("Worker average value: " + (matching groupBy { _.worker } map { case (ww, bids) => (ww, wmean( bids map { b => Estimate.byWeight(b.workload/b.timeload, b.timeload) } )) } ))
	  println("Job value: " + (matching map { b => b.workload/b.timeload }))
	  println("Average value: " + wmean( matching map { b => Estimate.byWeight(b.workload/b.timeload, b.timeload) } ))

	  println("Credit rates: " + Market.creditRate(matching))
	  println("Production rate: " + Market.productionRate(matching))
  }

  /**
   * Run a market-based simulation
   * @param numWorkers The number of workers
   * @param jobSize Average job size (FTE)
   * @param numDisciplines The number of distinct work disciplines that work requires (ie. the work diversity)
   * @param simYears The number of years to simulate
   * @param batchFreq The number of times per year that new jobs are released and bid upon
   * @param mTypes A list of market types to compare on the same sequence of jobs
   * @param numRuns The number of times to run the sim for each case
   * @return IndexedSeq<numRuns>(Iterable<mTypes>(List<years*batchFreq>(Tuple(List(Workers), List(Successful bids)))))
   */
  def comparativeMarketSim( numWorkers: Int, jobSize: Double, numDisciplines: Int, simYears: Double, batchFreq: Double, mTypes: Iterable[MarketType] = List(CreditMarket), numRuns: Int = 1 ) = {
    // Overall market process:
  	// Generate a batch of jobs, slightly exceeding requirement (workMin * workers)
  	// Allocate these jobs using marketBidding
  	// Determine unallocated jobs
  	// Generate additional jobs to slightly exceed requirement again
    // Determine worker commitment (and rates) from most recent set of allocations
    // Continue market bidding until max work reached
	val margin = 1.5
    val workStep = 1.0/batchFreq
    val batchWork = numWorkers/batchFreq*margin
    val numJobs = batchWork/jobSize toInt
    val market = new Market(numDisciplines)

    def marketSim( ws: List[Worker], js: Iterable[Job], moreJobs: Stream[Job], minWork: Double, mType: MarketType,
                   acc: List[(List[Worker], List[Bid])] ): List[(List[Worker], List[Bid])] = {
      /* println("Jobs: " + (js map {_.workload} sum))
      for (j <- js) println( j.id + ": " + j.skills ) */

      val (matching, workers0) = market.marketBidding(ws, js, minWork, mType)
	    
      // Update workers with current commitment, to a minimum of minWork, since that time has passed
      val tmap = Market.timeCommitment(matching)
	  val updatedWorkers = workers0 map { w => Worker(w.name, w.efficiency, w.rate, math.max(tmap.getOrElse(w, 0.0) + w.committed, minWork)) }

      val unfilledJobs = js.toSet &~ matching.map(_.job).toSet
      val newWork = numWorkers*workStep*margin - unfilledJobs.map(_.workload).sum
      val numJobs = newWork/jobSize toInt
      
      val (newJobs, nextJobs) = moreJobs.splitAt(numJobs)
      
      if (minWork < simYears)
        marketSim( updatedWorkers, unfilledJobs.toList:::newJobs.toList, nextJobs, minWork + workStep, mType, (updatedWorkers, matching)::acc )
      else
    	(updatedWorkers, matching)::acc
    }
    
	for (i <- 1 to numRuns) yield {
	  // Create a new sample of workers and jobs for each run
      val workers = (1 to numWorkers) map (i => Worker(i.toString, (1 to numDisciplines) map ( _ => 1.0/normRand(1.0, 0.2) abs ) toList )) toList

      /* println("Workers:")
      for (w <- workers) println( w.name + ": " + w.efficiency ) */

      // Create stream of jobs to consume, reused for all market types
	  val allJobs = jobStream( orphanJob(jobSize, jobSize/2.0, numDisciplines ) )
	  val (newJobs, nextJobs) = allJobs.splitAt(numJobs)
	  
	  mTypes map (marketSim( workers, newJobs, nextJobs, workStep, _, Nil ).reverse)
	}
  }
  
  // val fullSim = comparativeMarketSim( 5, 0.2, 5, 1.0, 4.0, List(CreditMarket), 1 )

  println("Full market matching:")
  val mTypes = List(CreditMarket, PreferenceMarket, RandomMarket)
  val fullSim = comparativeMarketSim( 50, 0.2, 10, 3.0, 4.0, mTypes, 1 )

  // Display comparative results from one run
  val fullResults = (fullSim.head zip mTypes) map {case (res, m) => collectResults(res, m)}

  // val comp = compareStats(fullSim.head.head, fullSim.head(1))

  
  for (res <- fullResults; (label, value) <- res) println(label + ": " + value)
  
  // Collect results of all runs for one market type and calculate stats
  val resultSeq = fullSim.transpose.head map (collectResults(_, mTypes(0)))
  
  println
  println("Run stats for CreditMarket")
  for ((label, value) <- resultStats( resultSeq )) println(label + ": " + value)
  
  // Think about: how to do comparative stats. Can compare different market types with same workers and jobs
  // And can compare over multiple runs.
  // For a particular market type want statistics of each measure: mean + std
  // Can also calculate statistics of comparative measures. eg. mean + std of: overall value A/overall value B

  // Introduce classes for (v, std) and (v, weight)
  /** Calculate statistics on a set of results from multiple values */
  def getStats(vs: Iterable[Any]) = vs.head match {
    // Can't match on Iterable[Type] because Type is erased, so test head element then extract all matching elements into a new Iterable
    case _ if vs.size equals 1 => vs.head
    case _: Int => {
      val ds: Iterable[Int] = for (v <- vs) yield v match { case d: Int => d }  
      Estimate(mean(ds), std(ds))
    }
    case _: Double => {
      val ds: Iterable[Double] = for (v <- vs) yield v match { case d: Double => d }  
      Estimate(mean(ds), std(ds))
    }
    case _: Estimate => {
      val ds: Iterable[Estimate] = for (v <- vs) yield v match { case d: Estimate => d }
      val sdev = ds map (_.s)
      (wmeanstd(ds), Estimate(mean(sdev), std(sdev)))
    }
    case (_: Estimate, _: Estimate) => {
      val ds: Iterable[(Estimate, Estimate)] = 
        for (v <- vs) yield v match { case e2: (Estimate, Estimate) => e2 }
      val (vstd, stdstd) = ds.unzip
      (wmeanstd(vstd), wmeanstd(stdstd))
    }
    case o => o
  }
  
  def compare[T](v1: T, v2: T): Any = (v1, v2) match {
    case (i1: Int, i2: Int) => i2.toDouble/i1
    case (d1: Double, d2: Double) => d2/d1
    case (e1: Estimate, e2: Estimate) => Estimate( e2.v/e1.v, math.sqrt(math.pow(e2.s/e1.v, 2) + math.pow(e1.s * e2.v/math.pow(e1.v, 2), 2) ) )
    case ((e1: Estimate, e2: Estimate), (f1: Estimate, f2: Estimate)) => (compare(e1, f1), compare(e2, f2))
  }
  
  /** Compare values from two separate runs */
  def compareStats(m1: Map[String, Any], m2: Map[String, Any]): Map[String, Any] = {
	for ((k, v) <- m1) yield k->compare(v, m2(k))
  }
  
  /* def resultStats(history: Map[String, Any]): Map[String, Any] = {
    (history groupBy {case (s, _) => s}) map {case (s, vs) => (s, getStats(vs))}
  } */
  def resultStats[T <% IndexedSeq[LinkedHashMap[String, Any]]](history: T): Map[String, Any] = {
    (history.flatten groupBy {case (s: String, _) => s}) map {case (s, vs) => (s, getStats(vs.unzip._2))}
  } 
  
  
  /* val (workerHistory, fullMarketMatch) = fullSim.head.unzip
  println
  for (batch <- fullMarketMatch) printBidStats(batch)
  println */

  // def std2weight: Option[Double] => Double = so => (so.flatMap {s => Some(1.0/math.pow(s,2))}).getOrElse(0.0)
  def std2weight: Double => Double = 1.0/math.pow(_,2)
  def vstd2vwt: ((Double, Double)) => (Double, Double) = { case (v, s) => (v, 1.0/math.pow(s,2)) }

  def stdError( ve: (Double, Double) ) = ve._1.toString + " +/- " + ve._2

  def collectResults( simOutput: List[(List[Worker], List[Bid])], mType: MarketType ) = {
    val (workerHistory, fullMarketMatch) = simOutput.unzip
	val results: LinkedHashMap[String, Any] = LinkedHashMap("Market Type" -> mType)
  
    // Determine various stats such as: worker rates over time, value over time, number of bids per job allocated
    val creditMap = fullMarketMatch map { Market.creditRate(_) }
  
    // Time series of credit rates for each worker
    val creditHistory = (creditMap flatten).groupBy { case (w, _) => w } map { case (w, vs) => (w, vs.unzip._2 ) }
    val creditRates = creditHistory.values map (getStats(_))
    val avCreditRate = getStats(creditRates)
  
    results ++= Map("creditMap" -> creditMap, "Credit History" -> creditHistory, "Credit Rates" -> creditRates)

    results ++= Map("Av credit rate" -> avCreditRate, 
      "Production rate" -> Market.productionRate(fullMarketMatch flatten),
      "Pay-off ratio (Job/Worker)" -> Market.payoffRatio(fullMarketMatch flatten) )
  
    // Number of bids per job, averaged over workers
    val avBids = getStats(fullMarketMatch.flatMap(m => (m map { _.worker } groupBy {w => w}) map { case (w, ws) => w.bids.length.toDouble/ws.size }))
  
    val numBids = (fullMarketMatch.flatten flatMap {_.worker.bids} toSet) size
    val numJobs = fullMarketMatch.flatten.size
    results ++= Map("Num bids" -> numBids, "Num jobs" -> numJobs, "Av bids per worker per job" -> avBids, 
      "Av bids per job" -> numBids.toDouble/numJobs)
  
    val totalWork = fullMarketMatch.flatten map {_.workload} sum
    val timeWorked = fullMarketMatch.flatten map {_.timeload} sum
    val workers = workerHistory.last

    results ++= Map("Total work allocated" -> totalWork, "Work allocated per worker" -> totalWork/workers.size,
      "Total time allocated" -> timeWorked, "Time allocated per worker" -> timeWorked/workers.size,
      "Overall value" -> totalWork/timeWorked)

    val timeCommitted = workers map {_.committed} sum
    
    results ++= Map("Time % unemployed" -> (timeCommitted - timeWorked)/timeWorked, "Time unemployed per worker" -> (timeCommitted - timeWorked)/workers.size)
  
    // Efficiency for each worker is their average credit rate per period divided by their efficiency on their strongest skill
    // Mean and standard deviation of efficiency history, for each worker
    val effRates = creditHistory map { case (w, rs) => { val maxeff = w.efficiency.max; (mean(rs)/maxeff, std(rs)/maxeff) } } toList
    // Average efficiency over all workers for entire simulation
    val avEff = getStats(effRates)

    results ++= Map("Efficiency rates" -> effRates, "Av efficiency" -> avEff) 
  }
  
  /* println("Num bids: " + numBids + " Num jobs: " + numJobs)
  println("Av bids per worker per job: " + avBids)
  println("Av bids per job: " + numBids.toDouble/numJobs)

  println("Workers: " + workers)
  println("Amount of allocated work: " + totalWork + " = " + totalWork/workers.size + " per worker")
  println("Amount of allocated time: " + timeWorked + " = " + timeWorked/workers.size + " per worker" )
  println("Overall value: " + totalWork/timeWorked)
  
  // println("Worker average value: " + (matching groupBy {case (_, (w, _)) => w} map { case (ww, jws) => (ww, wmean( jws map { case (j, (w, _)) => (j.workload/j.workerTime(w), j.workerTime(w)) } )) } ))
  // println("Job value: " + (matching map { case (j, (w, _)) => j.workload/j.workerTime(w) }))
  // println("Average value: " + wmean( matching map { case (j, (w, _)) => (j.workload/j.workerTime(w), j.workerTime(w)) } ))

  println("Time % unemployed: " + (timeCommitted - timeWorked)/timeWorked + " = " + (timeCommitted - timeWorked)/workers.size + " per worker" )

  println("Credit map: " + creditMap)
  println("Credit rates: " + creditRates + Market.creditRate(fullMarketMatch flatten))
  println("Average credit rate: " + stdError(avCreditRate) )
  println("Production rate: " + stdError(Market.productionRate(fullMarketMatch flatten)) )
  println("Pay-off ratio (Job/Worker): " + stdError(Market.payoffRatio(fullMarketMatch flatten)) )

  println("Efficiency rates: " + (effRates map {stdError(_)}).mkString(", ") )
  println("Av efficiency: " + stdError(avEff) )
  */

  /* val numDisciplines = 5
  
  val numWorkers = 5
  val workers = (1 to numWorkers) map (i => Worker(i.toString, (1 to numDisciplines) map ( _ => normRand(1.0, 0.2) abs ) toList )) toList

  val numJobs = 20
  val jobs = orphanJobs(numJobs, numWorkers, 0.4, numDisciplines)

  println("Workers:")
  for (w <- workers) println( w.name + ": " + w.efficiency )
  
  println("Jobs:")
  for (j <- jobs) println( j.id + ": " + j.skills )
  
  val m1 = new Market(numDisciplines)
  
  val oneToOne = m1.getMatching(workers, jobs)
  val oneToMany = m1.deferredAcceptance(workers, jobs)
  val randomMatch = m1.randomAllocation(workers, jobs)
  val marketMatch = m1.marketBidding(workers, jobs, 0.2)
  
  printStats(oneToOne)
  printStats(oneToMany)
  printStats(randomMatch)
  printStats1(marketMatch._1)
  */ 
  
}