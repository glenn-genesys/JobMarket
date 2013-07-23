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
   * A matching is stable if there is no pair (Job, Worker) that would both be better off matched with each other rather than their current matching
   * That is, a Job (or Worker) may prefer other workers (or jobs) to their current match, but those others must not likewise prefer the original job (or worker)
   * Make the assumption that if a worker is not allocated any jobs it is because they had no spare capacity, rather than that the algorithm failed
   * (since we no longer have spare capacity information).
   */
  def isStable(ws: List[Worker], bs: Iterable[Bid]) = {
    // Map of workers to their worst current assignment
    val worstJob = bs groupBy (_.worker) map {case (w, js) => (w, js.minBy( _.value ).job)}
    
    /** Would this worker prefer this job to any of their existing jobs? */
    // def isBetterJob( w: Worker, j: Job ) = worstJob.get(w).forall(w.jobPref(j) < w.jobPref(_))
    def isBetterJob( w: Worker, j: Job ) = worstJob.get(w).exists(w.jobPref(j) < w.jobPref(_))
    
    // What is the preferred order of workers, for each job
    val t0 = bs.map {case Bid(w, j, p) => j.workerPrefs(ws)}
    // Which workers are preferred more than the worker selected for each job
    val preferredWorkers = bs map {case Bid(w, j, p) => j.workerPrefs(ws).takeWhile( j.workerPref(_) > j.workerPref(w) )}
    // For each of these workers, find their preference for the same job and their preference for the worst job they are currently assigned
    val t000 = bs.map {case Bid(w, j, p) => j.workerPrefs(ws).takeWhile( j.workerPref(_) > j.workerPref(w) ).map( w1 => (w1.jobPref(j), worstJob.get(w1), worstJob.get(w1).map(w1.jobPref(_))) ) } 
    // For each of these workers, do they consider this job worse than any other jobs they are assigned
    val t1 = bs.map {case Bid(w, j, p) => j.workerPrefs(ws).takeWhile( j.workerPref(_) > j.workerPref(w) ).map( w1 => worstJob.get(w1).exists(w1.jobPref(j) > w1.jobPref(_)) ) } 
    val t2 = bs.map {case Bid(w, j, p) => j.workerPrefs(ws).takeWhile( j.workerPref(_) > j.workerPref(w) ).forall( w1 => worstJob.get(w1).exists(w1.jobPref(j) > w1.jobPref(_)) ) } 
    val t3 = bs.forall {case Bid(w, j, p) => j.workerPrefs(ws).takeWhile( j.workerPref(_) > j.workerPref(w) ).forall( w1 => worstJob.get(w1).exists(w1.jobPref(j) >= w1.jobPref(_)) ) }

    bs.forall {case Bid(w, j, p) => j.workerPrefs(ws).takeWhile( j.workerPref(_) > j.workerPref(w) ).forall( !isBetterJob(_, j) ) }
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
   * @return IndexedSeq<numRuns>(Iterable<mTypes>(Map[String,Any]))
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
    val market = new Market(mTypes.head, numWorkers, numDisciplines, workStep, margin, jobSize)

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
	  
	  mTypes map { m => collectResults(marketSim( workers, newJobs, nextJobs, workStep, m, Nil ).reverse, List(("Market Type", m))) }
	}
  }
  
  /**
   * Run sensitivity analysis for a given parameter in a market-based simulation
   * @param numWorkers The number of workers
   * @param jobSize Average job size (FTE)
   * @param numDisciplines The number of distinct work disciplines that work requires (ie. the work diversity)
   * @param simYears The number of years to simulate
   * @param batchFreq The number of times per year that new jobs are released and bid upon
   * @param mTypes A list of market types to compare on the same sequence of jobs
   * @param numRuns The number of times to run the sim for each case
   * @return IndexedSeq<numRuns>(Iterable<mTypes>(Map[String,Any]))
   */
  def sensitivityMarketSim( numWorkers: Int, jobSize: Double, numDisciplines: Int, simYears: Double, batchFreq: Double, mType: MarketType, numRuns: Int,
      paramType: String, paramRange: List[Any] ) = {
    // Overall market process:
  	// Generate a batch of jobs, slightly exceeding requirement (workMin * workers)
  	// Allocate these jobs using marketBidding
  	// Determine unallocated jobs
  	// Generate additional jobs to slightly exceed requirement again
    // Determine worker commitment (and rates) from most recent set of allocations
    // Continue market bidding until max work reached
    def marketSim( m: Market, ws: List[Worker], js: Iterable[Job], moreJobs: Stream[Job], minWork: Double, simYears: Double, mType: MarketType,
                   acc: List[(List[Worker], List[Bid])] ): List[(List[Worker], List[Bid])] = {
      /* println("Jobs: " + (js map {_.workload} sum))
      for (j <- js) println( j.id + ": " + j.skills ) */

      val (matching, workers0) = m.marketBidding(ws, js, minWork, mType)
	    
      // Update workers with current commitment, to a minimum of minWork, since that time has passed
      val tmap = Market.timeCommitment(matching)
	  val updatedWorkers = workers0 map { w => Worker(w.name, w.efficiency, w.rate, math.max(tmap.getOrElse(w, 0.0) + w.committed, minWork)) }

      val unfilledJobs = js.toSet &~ matching.map(_.job).toSet
      val newWork = m.numWorkers*m.workStep*m.margin - unfilledJobs.map(_.workload).sum
      val numJobs = newWork/m.jobSize toInt
      
      val (newJobs, nextJobs) = moreJobs.splitAt(numJobs)
      
      if (minWork < simYears)
        marketSim( m, updatedWorkers, unfilledJobs.toList:::newJobs.toList, nextJobs, minWork + m.workStep, simYears, mType, (updatedWorkers, matching)::acc )
      else
    	(updatedWorkers, matching)::acc
    }
    
    def collectSimResults(numWorkers: Int, jobSize: Double, numDisciplines: Int, simYears: Double, batchFreq: Double, mType: MarketType, margin: Double,
        paramType: String, p: Any) = {
    	val market = new Market(mType, numWorkers, numDisciplines, workStep = 1.0/batchFreq, margin, jobSize)

    	// val batchWork = numWorkers/batchFreq*margin
    	val firstJobs = numWorkers/batchFreq*margin/jobSize toInt

    	// Create a new sample of workers and jobs for each run
    	val workers = (1 to numWorkers) map (i => Worker(i.toString, (1 to numDisciplines) map ( _ => 1.0/normRand(1.0, 0.2) abs ) toList )) toList

    	// Create stream of jobs to consume, reused for all market types
    	val allJobs = jobStream( orphanJob(jobSize, jobSize/2.0, numDisciplines ) )
    	val (newJobs, nextJobs) = allJobs.splitAt(firstJobs)

    	collectResults(marketSim( market, workers, newJobs, nextJobs, market.workStep, simYears, mType, Nil ).reverse, List((paramType, p)))
    }
    
	for (i <- 1 to numRuns) yield {
	    val margin = 1.5

	    paramType match {
	      case "numWorkers" => {
	        val pRange = paramRange map { case i:Int => i }
	        
	        for (p <- pRange) yield {
	          // collectSimResults(p, jobSize, numDisciplines, simYears, batchFreq, mType, margin, paramType, p)
		        val market = new Market(mType, p, numDisciplines, workStep = 1.0/batchFreq, margin, jobSize)
		        
			    // val batchWork = numWorkers/batchFreq*margin
			    val firstJobs = p/batchFreq*margin/jobSize toInt
		
			    // Create a new sample of workers and jobs for each run
			    val workers = (1 to p) map (i => Worker(i.toString, (1 to numDisciplines) map ( _ => 1.0/normRand(1.0, 0.2) abs ) toList )) toList
		
			    // Create stream of jobs to consume, reused for all market types
			    val allJobs = jobStream( orphanJob(jobSize, jobSize/2.0, numDisciplines ) )
			    val (newJobs, nextJobs) = allJobs.splitAt(firstJobs)

			    collectResults(marketSim( market, workers, newJobs, nextJobs, market.workStep, simYears, mType, Nil ).reverse, List((paramType, p)))
			}
	      }
	      case "jobSize" => {
	        val pRange = paramRange map { case i:Double => i }
	        
	        for (p <- pRange) yield {
		        val market = new Market(mType, numWorkers, numDisciplines, workStep = 1.0/batchFreq, margin, p)
		        
			    // val batchWork = numWorkers/batchFreq*margin
			    val firstJobs = numWorkers/batchFreq*margin/p toInt
		
			    // Create a new sample of workers and jobs for each run
			    val workers = (1 to numWorkers) map (i => Worker(i.toString, (1 to numDisciplines) map ( _ => 1.0/normRand(1.0, 0.2) abs ) toList )) toList
		
			    // Create stream of jobs to consume, reused for all market types
			    val allJobs = jobStream( orphanJob(p, p/2.0, numDisciplines ) )
			    val (newJobs, nextJobs) = allJobs.splitAt(firstJobs)

			    collectResults(marketSim( market, workers, newJobs, nextJobs, market.workStep, simYears, mType, Nil ).reverse, List((paramType, p)))
			}
	      }
	      case "numDisciplines" => {
	        val pRange = paramRange map { case i:Int => i }
	        
	        for (p <- pRange) yield {
		        val market = new Market(mType, numWorkers, p, workStep = 1.0/batchFreq, margin, jobSize)
		        
			    // val batchWork = numWorkers/batchFreq*margin
			    val firstJobs = numWorkers/batchFreq*margin/jobSize toInt
		
			    // Create a new sample of workers and jobs for each run
			    val workers = (1 to numWorkers) map (i => Worker(i.toString, (1 to p) map ( _ => 1.0/normRand(1.0, 0.2) abs ) toList )) toList
		
			    // Create stream of jobs to consume, reused for all market types
			    val allJobs = jobStream( orphanJob(jobSize, jobSize/2.0, p ) )
			    val (newJobs, nextJobs) = allJobs.splitAt(firstJobs)

			    collectResults(marketSim( market, workers, newJobs, nextJobs, market.workStep, simYears, mType, Nil ).reverse, List((paramType, p)))
			}
	      }
	      case "batchFreq" => {
	        val pRange = paramRange map { case i:Int => i }
	        
	        for (p <- pRange) yield {
		        val market = new Market(mType, numWorkers, numDisciplines, workStep = 1.0/p, margin, jobSize)
		        
			    // val batchWork = numWorkers/batchFreq*margin
			    val firstJobs = numWorkers/p*margin/jobSize toInt
		
			    // Create a new sample of workers and jobs for each run
			    val workers = (1 to numWorkers) map (i => Worker(i.toString, (1 to numDisciplines) map ( _ => 1.0/normRand(1.0, 0.2) abs ) toList )) toList
		
			    // Create stream of jobs to consume, reused for all market types
			    val allJobs = jobStream( orphanJob(jobSize, jobSize/2.0, numDisciplines ) )
			    val (newJobs, nextJobs) = allJobs.splitAt(firstJobs)

			    collectResults(marketSim( market, workers, newJobs, nextJobs, market.workStep, simYears, mType, Nil ).reverse, List((paramType, p)))
			}
	      }
	      case "margin" => {
	        val pRange = paramRange map { case i:Double => i }
	        
	        for (p <- pRange) yield {
		        val market = new Market(mType, numWorkers, numDisciplines, workStep = 1.0/batchFreq, p, jobSize)
		        
			    // val batchWork = numWorkers/batchFreq*margin
			    val firstJobs = numWorkers/batchFreq*p/jobSize toInt
		
			    // Create a new sample of workers and jobs for each run
			    val workers = (1 to numWorkers) map (i => Worker(i.toString, (1 to numDisciplines) map ( _ => 1.0/normRand(1.0, 0.2) abs ) toList )) toList
		
			    // Create stream of jobs to consume, reused for all market types
			    val allJobs = jobStream( orphanJob(jobSize, jobSize/2.0, numDisciplines ) )
			    val (newJobs, nextJobs) = allJobs.splitAt(firstJobs)

			    collectResults(marketSim( market, workers, newJobs, nextJobs, market.workStep, simYears, mType, Nil ).reverse, List((paramType, p)))
			}
	      }
	      /* case "template" => {
	        val pRange = paramRange map { case i:Int => i }
	        
	        for (p <- pRange) yield {
		        val market = new Market(mType, numWorkers, numDisciplines, workStep = 1.0/batchFreq, margin, jobSize)
		        
			    // val batchWork = numWorkers/batchFreq*margin
			    val firstJobs = numWorkers/batchFreq*margin/jobSize toInt
		
			    // Create a new sample of workers and jobs for each run
			    val workers = (1 to numWorkers) map (i => Worker(i.toString, (1 to numDisciplines) map ( _ => 1.0/normRand(1.0, 0.2) abs ) toList )) toList
		
			    // Create stream of jobs to consume, reused for all market types
			    val allJobs = jobStream( orphanJob(jobSize, jobSize/2.0, numDisciplines ) )
			    val (newJobs, nextJobs) = allJobs.splitAt(firstJobs)

			    collectResults(marketSim( market, workers, newJobs, nextJobs, market.workStep, simYears, mType, Nil ).reverse, List((paramType, p)))
			}
	      } */
	  }
	}
  }
  
  /** Calculate statistics on a set of results from multiple values */
  def getStats(vs: Iterable[Any]) = vs.head match {
    // Can't match on Iterable[Type] because Type is erased, so test head element then extract all matching elements into a new Iterable
    case _ if vs.size equals 1 => vs.head
    case _: Int => {
      val ds: Iterable[Int] = for (v <- vs) yield v match { case d: Int => d }  
      Estimate(ds)
    }
    case _: Double => {
      val ds: Iterable[Double] = for (v <- vs) yield v match { case d: Double => d }  
      Estimate(ds)
    }
    case _: Estimate => {
      val ds: Iterable[Estimate] = for (v <- vs) yield v match { case d: Estimate => d }
      val sdev = ds map (_.u)
      (Estimate(ds), Estimate(sdev))
    }
    case (_: Estimate, _: Estimate) => {
      val ds: Iterable[(Estimate, Estimate)] = 
        for (v <- vs) yield v match { case e2: (Estimate, Estimate) => e2 }
      val (vstd, stdstd) = ds.unzip
      (Estimate(vstd), Estimate(stdstd))
    }
    case o => o
  }
  
  /** Estimate a trend on a set of results from multiple values */
  def getTrend(vs: Iterable[Any]) = vs.head match {
    // Place holder -- need to do some linear algebra
    case _ => vs
  }
  
  def compare[T](v1: T, v2: T): Any = (v1, v2) match {
    case (i1: Int, i2: Int) => i2.toDouble/i1
    case (d1: Double, d2: Double) => d2/d1
    case (e1: Estimate, e2: Estimate) => Estimate( e2.v/e1.v, math.sqrt(math.pow(e2.u/e1.v, 2) + math.pow(e1.u * e2.v/math.pow(e1.v, 2), 2) ) )
    case ((e1: Estimate, e2: Estimate), (f1: Estimate, f2: Estimate)) => (compare(e1, f1), compare(e2, f2))
    case o => o
  }
  
  /** Compare values from two separate runs */
  def compareStats(m1: Iterable[(String,Any)], m2: Map[String,Any]): Map[String, Any] = {
    ( for (((k, v)) <- m1) yield k->compare(v, m2(k)) ) toMap
  }
  
  def resultStats(history: Iterable[Map[String, Any]]): Map[String, Any] = {
    (history.flatten groupBy {case (s: String, _) => s}) map {case (s, vs) => (s, getStats(vs.unzip._2))}
  }
  
  def trendAnalysis(history: Iterable[Map[String, Any]]): Map[String, Any] = {
    (history.flatten groupBy {case (s: String, _) => s}) map {case (s, vs) => (s, getTrend(vs.unzip._2))}
  }
  
  /* val (workerHistory, fullMarketMatch) = fullSim.head.unzip
  println
  for (batch <- fullMarketMatch) printBidStats(batch)
  println */

  // def std2weight: Option[Double] => Double = so => (so.flatMap {s => Some(1.0/math.pow(s,2))}).getOrElse(0.0)
  def std2weight: Double => Double = 1.0/math.pow(_,2)
  def vstd2vwt: ((Double, Double)) => (Double, Double) = { case (v, s) => (v, 1.0/math.pow(s,2)) }

  def stdError( ve: (Double, Double) ) = ve._1.toString + " +/- " + ve._2

  /**
   * Takes the simulation output from a single run, for a single market type, calculates various metrics and returns them in a map 
   * @param simOutput The output from a single run (multiple rounds of job releases) of a market simulation
   * @param mType The market type
   * @return Map of String to results (Any)
   */
  def collectResults( simOutput: List[(List[Worker], List[Bid])], params: List[(String, Any)] ) = {
    val (workerHistory, fullMarketMatch) = simOutput.unzip
	val results: LinkedHashMap[String, Any] = params.foldLeft(LinkedHashMap.empty[String, Any])( _ += _ )

	results ++= Map("Stable matchings" -> ((workerHistory, fullMarketMatch).zipped map { case (ws, mm) => isStable(ws, mm)} ) )
	
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
    
    // Calculate mean supply and demand of each discipline
    /* val demand = fullMarketMatch.map(_.map(_.job.skills).transpose map { Estimate(_) }) map { Estimate(_) }
    val supply = workerHistory.last.map(_.efficiency).transpose map { Estimate(_) } */
    // Estimate supply and demand curves
    // val demand = fullMarketMatch.flatten.map(_.job.skills).transpose map { Histogram(_) }
    val demand = fullMarketMatch.flatten.map(b => b.job.skills map (s => (s, b.job.workload))).transpose map { dd => val (v,w) = dd.unzip; Histogram(v, w) }
    val supply = workerHistory.last.map(_.efficiency).transpose map { Histogram(_) }
    
    results // ++= Map("Supply per skill" -> supply, "Demand per skill" -> demand)
    
    // Want to calculate 'market-value' of each discipline -- but how?
    // Rather than mean supply and demand it may be more useful to find the cumulative histograms 
    // -- this may deliver a supply curve, which can be compared to the demand curve to find the equilibrium?

  }
  
  println("Sensitivity analysis:")

  /* val fullSim = sensitivityMarketSim( numWorkers=20, jobSize = 0.2, numDisciplines=10, simYears=3.0, batchFreq=4.0, mType=CreditMarket, numRuns=20,
      paramType = "numWorkers", paramRange = 10 to 100 by 10 toList ) */

  /* val fullSim = sensitivityMarketSim( numWorkers=20, jobSize = 0.2, numDisciplines=10, simYears=3.0, batchFreq=4.0, mType=CreditMarket, numRuns=20,
      paramType = "jobSize", paramRange = List(0.1, 0.2, 0.3, 0.5, 0.7, 0.9))  // , 1.0, 1.5) )   // Overall value decreases slightly  */

  /* val fullSim = sensitivityMarketSim( numWorkers=20, jobSize = 0.2, numDisciplines=10, simYears=3.0, batchFreq=4.0, mType=CreditMarket, numRuns=20,
      paramType = "numDisciplines", paramRange = List(1, 2, 5, 10, 20, 30, 50) )   // Results not highly sensitive to numDisciplines   */

  /* val fullSim = sensitivityMarketSim( numWorkers=20, jobSize = 0.2, numDisciplines=10, simYears=3.0, batchFreq=4.0, mType=CreditMarket, numRuns=20,
      paramType = "batchFreq", paramRange = List(1, 2, 4, 12))  // , 26, 52) )  // Unemployed time increases, overall value decreases, efficiency peaks at f=2 */
  
  val fullSim = sensitivityMarketSim( numWorkers=20, jobSize = 0.2, numDisciplines=10, simYears=3.0, batchFreq=4.0, mType=CreditMarket, numRuns=20,
      paramType = "margin", paramRange = 0.9 to 2.0 by 0.2 toList)   // Very high unemployment % for low values. 1.5 -> < 1%. 
      // May indicate a problem in the model. Must measure the amount of time it takes jobs to be filled, and/or #jobs that are never filled 
  
  // for (res <- fullSim.head; (label, value) <- res) println(label + ": " + value)

  /* println("Full market matching:")
  val mTypes = List(CreditMarket, PreferenceMarket, RandomMarket)
  val fullSim = comparativeMarketSim( numWorkers=50, jobSize=0.2, numDisciplines=10, simYears=3.0, batchFreq=4.0, mTypes=mTypes, numRuns=20 )
  // val fullSim = comparativeMarketSim( numWorkers=5, jobSize=0.2, numDisciplines=2, simYears=1.0, batchFreq=4.0, mTypes=List(PreferenceMarket), numRuns=1 )

  // Display comparative results from one run

  val comp = fullSim.transpose map {_.take(2).toList match {case List(r1,r2) => compareStats(r1, r2 toMap); case _ => Map.empty[String, Any]}}

  println()
  println("Comparative stats for " + mTypes.take(2))
  for ((label, value) <- resultStats( comp )) println(label + ": " + value)
  */
  
  // Collect results of all runs for each market type and calculate stats
  
  val resultList = for (resultSeq <- fullSim.transpose) yield {
  	println
  	println("Run stats for each market")
  	
  	val runResults = resultStats( resultSeq map (_ toMap) )
  	
  	// for ((label, value) <- runResults) println(label + ": " + value)
  	runResults
  }  
  
  val trends = trendAnalysis(resultList)
  
  println
  println("Trend spotting:")
  for ((label, value) <- trends) println(label + ": " + value)

  
  // Think about: how to do comparative stats. Can compare different market types with same workers and jobs
  // And can compare over multiple runs.
  // For a particular market type want statistics of each measure: mean + std
  // Can also calculate statistics of comparative measures. eg. mean + std of: overall value A/overall value B


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