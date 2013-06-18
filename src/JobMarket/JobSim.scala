package JobMarket

import util.Random
import scala.collection.immutable.Map

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
  def wmean(xs:Iterable[(Double,Double)]): Double = (xs.foldLeft((0.0, 0.0)) { case ((vs, ws), (v, w)) => (vs + v*w, ws + w) } ) match {case (vs,ws) => vs/ws}

  /** Calculate the weighted mean and estimate the population standard deviation */
  def wmeanstd( wvs: Iterable[(Double, Double)] ) = (wmean(wvs), std(wvs map {_._1}))
  
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

	  println("Worker average value: " + (matching groupBy { _._2 } map { case (ww, jws) => (ww, wmean( jws map { case (j, w) => (j.workload/j.workerTime(w), j.workerTime(w)) } )) } ))
	  println("Job value: " + (matching map { case (j, w) => j.workload/j.workerTime(w) }))
	  println("Average value: " + wmean( matching map { case (j, w) => (j.workload/j.workerTime(w), j.workerTime(w)) } ))
  }

  def printStats1( matching: List[Bid] ) {
	  val totalWork = matching map {_.workload} sum
	  val timeWorked = matching map { _.timeload } sum
	  val numWorkers = (matching map {_.worker} toSet).size

      println("1 to n matching= " + matching)
	  println("1 to n commitment= " + Market.timeCommitment(matching))

	  // println("Number of unmatched jobs: " + (numJobs - matching.size) + "/" + numJobs)
	  println("Amount of allocated work: " + totalWork + " = " + totalWork/numWorkers + " per worker")
	  println("Amount of allocated time: " + timeWorked + " = " + timeWorked/numWorkers + " per worker" )

	  println("Worker average value: " + (matching groupBy { _.worker } map { case (ww, bids) => (ww, wmean( bids map { b => (b.workload/b.timeload, b.timeload) } )) } ))
	  println("Job value: " + (matching map { b => b.workload/b.timeload }))
	  println("Average value: " + wmean( matching map { b => (b.workload/b.timeload, b.timeload) } ))

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
   * @return ?? 
   */
  def marketSim( numWorkers: Int, jobSize: Double, numDisciplines: Int, simYears: Double, batchFreq: Double ) = {
    // Overall market process:
  	// Generate a batch of jobs, slightly exceeding requirement (workMin * workers)
  	// Allocate these jobs using marketBidding
  	// Determine unallocated jobs
  	// Generate additional jobs to slightly exceed requirement again
    // Determine worker commitment (and rates) from most recent set of allocations
    // Continue market bidding until max work reached
	val margin = 2.0
    val workers = (1 to numWorkers) map (i => Worker(i.toString, (1 to numDisciplines) map ( _ => 1.0/normRand(1.0, 0.2) abs ) toList )) toList
    val workStep = 1.0/batchFreq
    val batchWork = numWorkers/batchFreq*margin
    val numJobs = batchWork/jobSize toInt
    val market = new Market(numDisciplines)

    println("Workers:")
    for (w <- workers) println( w.name + ": " + w.efficiency )
  
    // Create stream of jobs to consume
    val alljobs = jobStream( orphanJob(jobSize, jobSize/2.0, numDisciplines ) )

    val (newJobs, nextJobs) = alljobs.splitAt(numJobs)
    
    def marketSim( ws: List[Worker], js: List[Job], moreJobs: Stream[Job], minWork: Double, 
                   acc: List[(List[Worker], List[Bid])] ): List[(List[Worker], List[Bid])] = {
      println("Jobs: " + (js map {_.workload} sum))
      for (j <- js) println( j.id + ": " + j.skills )

      val (matching, workers0) = market.marketBidding(ws, js, minWork)
	    
      val tmap = Market.timeCommitment(matching)
	  val updatedWorkers = workers0 map { w => Worker(w.name, w.efficiency, w.rate, tmap.getOrElse(w, 0.0) + w.committed) }

      val unfilledJobs = js.toSet &~ matching.map(_.job).toSet
      val newWork = numWorkers*workStep*margin - unfilledJobs.map(_.workload).sum
      val numJobs = newWork/jobSize toInt
      
      val (newJobs, nextJobs) = moreJobs.splitAt(numJobs)
      
      if (minWork < simYears)
        marketSim( updatedWorkers, unfilledJobs.toList:::newJobs.toList, nextJobs, minWork + workStep, (updatedWorkers, matching)::acc )
      else
    	(updatedWorkers, matching)::acc
    }
    
    marketSim( workers, newJobs toList, nextJobs, workStep, Nil ).reverse
  }
  
  println("Full market matching:")
  val (workerHistory, fullMarketMatch) = marketSim( 50, 0.2, 10, 3.0, 4.0 ).unzip
  println
  for (batch <- fullMarketMatch) printStats1(batch)
  println

  // Determine various stats such as: worker rates over time, value over time, number of bids per job allocated
  val creditMap = fullMarketMatch map { Market.creditRate(_) }
  // Time series of credit rates for each worker
  val creditHistory = (creditMap flatten).groupBy { _._1 } map { case (w, vs) => (w, vs map (_._2)) }
  val creditRates = creditHistory.values map { rs => (mean(rs), std(rs)) }
  val avCreditRate = ( wmean(creditRates map { case (r, stdr) => (r, 1.0/math.pow(stdr,2)) }),
		  		       std(creditRates map (_._1)))
  
  // Number of bids per job, averaged over workers
  val avBids = mean(fullMarketMatch.flatMap(m => (m map { _.worker } groupBy {w => w}) map { case (w, ws) => w.bids.length.toDouble/ws.size }))
  
  val numBids = (fullMarketMatch.flatten flatMap {_.worker.bids} toSet) size
  val numJobs = fullMarketMatch.flatten size
  
  println("Num bids: " + numBids + " Num jobs: " + numJobs)
  println("Av bids per worker per job: " + avBids)
  println("Av bids per job: " + numBids.toDouble/numJobs)
  println("Credit history: " + creditHistory)
  
  val totalWork = fullMarketMatch.flatten map {_.workload} sum
  val timeWorked = fullMarketMatch.flatten map {_.timeload} sum
  val workers = workerHistory.last

  // println("1 to n matching= " + matching)
  println("Worker commitment= " + workers.map (_.committed) )

  println("Workers: " + workers)
  println("Amount of allocated work: " + totalWork + " = " + totalWork/workers.size + " per worker")
  println("Amount of allocated time: " + timeWorked + " = " + timeWorked/workers.size + " per worker" )
  println("Overall value: " + totalWork/timeWorked)

  // println("Worker average value: " + (matching groupBy {case (_, (w, _)) => w} map { case (ww, jws) => (ww, wmean( jws map { case (j, (w, _)) => (j.workload/j.workerTime(w), j.workerTime(w)) } )) } ))
  // println("Job value: " + (matching map { case (j, (w, _)) => j.workload/j.workerTime(w) }))
  // println("Average value: " + wmean( matching map { case (j, (w, _)) => (j.workload/j.workerTime(w), j.workerTime(w)) } ))

  def stdError( ve: (Double, Double) ) = ve._1.toString + " +/- " + ve._2
  
  println("Credit rates: " + creditRates + Market.creditRate(fullMarketMatch reduceLeft { _ ++ _ }))
  println("Average credite rate: " + stdError(avCreditRate)) 
  println("Production rate: " + stdError(Market.productionRate(fullMarketMatch reduceLeft { _ ++ _ })) )
  println("Pay-off ratio (Job/Worker): " + stdError(Market.payoffRatio(fullMarketMatch reduceLeft { _ ++ _ })) )
  
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