package JobMarket

import scala.collection.immutable.Map
import util.Random
import JobSim.{wmean,std}

/**
 * @author burgessg
 *
 * The market coordinates job offers and bids and simulates the matching process
 */
object Market {
  /** 
   * Calculate the time commitment for each worker implied by the given matching
   * Return the result in a Map
   */
  def commitment( m:Map[Job, Worker] ) = 
    m groupBy { _._2 } map { case (ww, jws) => (ww, jws map { case (j, w) => j.workerTime(w) } sum ) }

  /** 
   * Calculate the time commitment for each worker implied by the given matching
   * Return the result in a Map
   */
  def timeCommitment( m:Map[Job, Bid] ) = 
    m.values groupBy { _.worker } map { case (w, bids) => (w, bids map { _.timeload } sum ) }
  /* def timeCommitment( m:Map[Job, (Worker, Double)] ) = 
    m groupBy { _._2._1 } map { case (ww, jws) => (ww, jws map { case (j, (w, _)) => j.workerTime(w) } sum ) }
    */

  /** 
   * Calculate the average crediting rate for each worker implied by the given matching
   * Return the result in a Map
   */
  def creditRate( m:Map[Job, Bid] ) = 
    m.values groupBy { _.worker } map { case (ww, bids) => (ww, wmean( bids map { b => (b.price/b.timeload, b.timeload) } )) }
  /* def creditRate( m:Map[Job, (Worker, Double)] ) = 
    m groupBy { _._2._1 } map { case (ww, jws) => (ww, wmean( jws map { case (j, (w, p)) => (p/j.workerTime(w), j.workerTime(w)) } )) }
    */

 /** 
   * Calculate the average production rate over all jobs (ie. work/credit) implied by the given matching
   */
  def productionRate( m:Map[Job, Bid] ) = 
  { wvs: Iterable[(Double, Double)] => (wmean(wvs), std(wvs map {_._1})) }.apply( m.values map { case b: Bid => (b.workload/b.price, b.workload) } )
  /* def productionRate( m:Map[Job, (Worker, Double)] ) = 
  { wvs: Map[Double, Double] => (wmean(wvs), std(wvs map {_._1})) }.apply( m map { case (j, (w, p)) => (j.workload/p, j.workload) } ) */
   //  wmean(m map { case (j, (w, p)) => (j.workload/p, j.workload) })

 /** 
   * Calculate the average payoff-ratio over all jobs (ie. productionRate/creditRate) implied by the given matching
   */
  def payoffRatio( m:Map[Job, Bid] ) = 
  { wvs: Iterable[(Double, Double)] => (wmean(wvs), std(wvs map {_._1})) }.apply( m.values map { case b: Bid => (b.workload*b.timeload/math.pow(b.price, 2), b.workload) } )
  // { wvs: Map[Double, Double] => (wmean(wvs), std(wvs map {_._1})) }.apply( m map { case (j, (w, p)) => (j.workload*j.workerTime(w)/math.pow(p, 2), j.workload) } )


  /**
   * Generate bids of workers for the given jobs, using the supplied bid function: (Worker (Job Value)) => (Worker (Job Price))
   * But new bids must be different from all previous bids. In particular, bids for the same job should be *better* (ie. lower)
   * @param ws A list of workers to make new bids
   * @param js A list of jobs to bid for
   * @param current Current offers (workers don't rebid jobs they currently have an unrejected offer for)
   * @param bidfun A function that a worker uses to determine a bid price
   * @return A list of bids (one per worker)
   */
  def workerBids( ws: List[Worker], js: List[Job], current: Map[Job, Bid] ) = {
	  // For each worker, sort jobs in order of decreasing surplus value / time
	  // Surplus value is bid - rate*time, where bid is halfway between value and rate *or* 5% less than last bid
	  // Therefore, just need job with highest (new) bid / time
  
      // Workers do not rebid for which jobs they still have a current offer, so for each worker, exclude those they have bids for
	  // val allBids = ws.map( w => (w, js.filterNot { current.get(_).forall { _.worker equals w } } map { j =>
	  // 	Bid(w, j, (w.bids.collectFirst { case b: Bid => b.price*0.95 }).getOrElse((j.workload + j.workerTime(w)*w.rate)/2.0) ) } ) )
  
	  val allBids = ws.map( w => (w, js filter { !current.get(_).exists { _.worker equals w } } map { j =>
	  	Bid(w, j, (w.bids.collectFirst { case b: Bid => b.price*0.95 }).getOrElse((j.workload + j.workerTime(w)*w.rate)/2.0) ) } ) )
	  allBids map { case (w, bs) => bs maxBy { case Bid(w, j, p) => p/j.workerTime(w) } }
  }
  
  /**
   * Update the current set of matches (Job to Worker, with price) given a new set of costed bids
   * @param workerBids List of workers paired with their bids (worker (job, bid))
   * @param bids New list of bids for jobs (with price), by worker
   * @param current Current map of best (Worker) proposals made to each Job, and their price
   * @param workerPref Is a function Bid => Double that is highest for the bid most preferred by Job
   * i.e. It is the function that determines how jobs choose between bids based on price
   * @return Updated current best map of jobs to unrejected bids
   */
  def considerOffers( workerBids: List[Bid], 
		  			  workerPref: Bid => Double, 
		  			  current: Map[Job, Bid] ): Map[Job, Bid] = {
	  // Must force each job to choose their best offer
      (current.toList ::: (workerBids map { b => (b.job, b) })).sortBy { case (_, b) => workerPref(b) } toMap
  }
  
  /* def considerOffers( workerBids: List[(Worker, (Job, Double))], 
		  			  workerPref: ((Job, (Worker, Double))) => Double, 
		  			  current: Map[Job, (Worker, Double)] ): Map[Job, (Worker, Double)] = {
      // Reformat bids, concat to current offers and choose best worker for each job
      (current.toList ::: (workerBids map { case (w, (j, p)) => (j, (w, p)) })).sortBy(workerPref)_ toMap
  } */
  /* def considerOffers( workerBids: List[(Worker, (Job, Double))], 
		  			  workMax: Double, 
		  			  workerPref: ((Job, (Worker, Double))) => Double, 
		  			  current: Map[Job, (Worker, Double)] ): Map[Job, (Worker, Double)] = {
      // bids is list of next bids made by corresponding workers
      // Make list of all current offers for each job
      val currentCommitment = timeCommitment(current)

      val newOffers = for { 
        (w, (j, p)) <- workerBids 
        val cc: Double =  w.committed + (currentCommitment getOrElse (w, 0.0))
        if cc + j.skills.sum <= workMax     // Exclude over-committed workers
      } yield (j, (w, p))

      if (newOffers.isEmpty) return current

      val allOffers = current.toList ::: newOffers
      // Choose best
      val bestOffers = allOffers.sortBy(workerPref)_ toMap
      
      bestOffers
  } */

}


class Market( disciplines: Int ) {
  
	def getMatching( ws: List[Worker], js: List[Job] ) = {
	  // Get job preferences of all workers. ie. the jobs that workers prefer
	  val jobPrefs = ws.map( _.jobPrefs(js) )
	  
	  // And get worker preferences for all jobs. ie. the workers that are best for each job
	  // val workerPrefs = js.map( j => (j -> j.workerPrefs(ws)) ) toMap
	  val workerPrefs = js.map( _.workerPrefs(ws) )

	  // Offers are made in rounds: each worker starts with their first preference Job
	  val offers = jobPrefs.transpose
	  
	/**
	 * Make one set of stable matches, using worker-proposing algorithm (which favours workers)
	 * @param offers Remaining rounds of offers
	 * @param current Current map of best (Worker) proposals made to each Job
	 * @return Final matching
	 */
	def makeOffers( offers: List[List[Job]], current: Map[Job, Worker] ): Map[Job, Worker] = offers match {
	    case Nil => current
	    case next :: t => {
	      // next is list of next proposals made by corresponding workers
	      // Make list of all current offers for each job
	      val newOffers = for { (j, wi) <- next.zipWithIndex
	    	  if !current.values.toSet(ws(wi))     // Only get new offers from unmatched workers
	        } yield (j, ws(wi))
	        
	      val allOffers = current.toList ::: newOffers
	      // Choose best
	      val bestOffers = allOffers.sortBy { case (j, w) => j.workerPref(w) } toMap
	      
	      makeOffers(t, bestOffers)
	    }
	  }

	  
	makeOffers(offers, Map.empty[Job, Worker])
  }          
    
  /**
   * Make set of many to 1 stable matches (Job to Worker), using worker-proposing algorithm (which favours workers)
   * @param offers Remaining rounds of offers
   * @param current Current map of best (Worker) proposals made to each Job
   * @return Final matching
   */
  def makeMoreOffers( ws: List[Worker], workLimit: Double, workerPref: ((Job, Worker)) => Double, offers: List[List[Job]], current: Map[Job, Worker] ): Map[Job, Worker] = offers match {
    case Nil => current
    case next :: t => {
      // next is list of next proposals made by corresponding workers
      // Make list of all current offers for each job
      val currentCommitment = Market.commitment(current)

      val newOffers = for { 
        (j, wi) <- next.zipWithIndex 
        val cc: Double = currentCommitment getOrElse (ws(wi), 0.0)
        if cc + j.skills.sum <= workLimit     // Exclude over-committed workers
      } yield (j, ws(wi))

      if (newOffers.isEmpty) return current

      val allOffers = current.toList ::: newOffers
      // Choose best
      val bestOffers = allOffers.sortBy(workerPref)_ toMap

      makeMoreOffers(ws, workLimit, workerPref, t, bestOffers)
    }
  }

  def deferredAcceptance( ws: List[Worker], js: List[Job] ) = {
	  // Get job preferences of all workers. ie. the jobs that workers prefer
	  val jobPrefs = ws.map( _.jobPrefs(js) )
	  
	  // And get worker preferences for all jobs. ie. the workers that are best for each job
	  // val workerPrefs = JobSim.funPrint(js.map( _.workerPrefs(ws) ))

	  // Offers are made in rounds: each worker starts with their first preference Job
	  val offers = jobPrefs.transpose
	  
	makeMoreOffers(ws, 1.0, { case (j, w) => j.workerPref(w) }, offers, Map.empty[Job, Worker])
  }          
    
  def randomAllocation( ws: List[Worker], js: List[Job] ) = {
	  // Get job preferences of all workers. ie. the jobs that workers prefer
	  val jobPrefs = for (w <- ws) yield Random.shuffle(js)
	  
	  // And get worker preferences for all jobs. ie. the workers that are best for each job
	  // val workerPrefs = JobSim.funPrint(js.map( _.workerPrefs(ws) ))

	  // Offers are made in rounds: each worker starts with their first preference Job
	  val offers = jobPrefs.transpose
	  
	makeMoreOffers(ws, 1.0, { _ => 1.0 }, offers, Map.empty[Job, Worker])
  }          
    
  /**
   * Simulate a market bidding process where workers bid for jobs
   * @param ws List of Workers
   * @param js List of Jobs
   * @param minWork Minimum workload each worker must try to commit to
   * @return Map matching Jobs to Workers, and updated list of workers
   */
  def marketBidding( inws: List[Worker], js: List[Job], minWork: Double ) = {
    
	  def marketBidding( ws: List[Worker], 
	                     bidders: List[Worker], 
	                     js: List[Job], 
	                     offers: Map[Job, Bid] ): (Map[Job, Bid], List[Worker]) = {
	      // Get bids of given workers for given jobs
		  val bids = Market.workerBids(bidders, js, offers)
	
	      // Update current best offers
		  val current = Market.considerOffers( bids, { b: Bid => 1.0/b.price }, offers )  
		  
		  // Bidding stops when no new bids are accepted in a round
		  // if (current equals offers) return (current, ws)
		  
		  val tmap = Market.timeCommitment(current)
		  
		  // Determine the average crediting rate implied by current allocation
		  val cmap = Market.creditRate(current)
		  
		  // Update crediting rate of workers according to whether their bid was rejected or not
		  // Committed is only updated once bids are finalised (at the end of this job cycle)
		  // If a worker is not found in cmap, then all of their bids have been rejected -- revert to original rate
		  val updatedWorkers = ws map { w => Worker(w.name, w.efficiency, cmap.getOrElse(w, inws.find(_ equals w).getOrElse(w).rate), w.committed,
		    										(bids collect { case b: Bid if b.worker equals w => b }):::w.bids ) }
		  
		  // Partition into those workers that need to rebid, and those that can sit
		  val (sitters, rebidders) = updatedWorkers partition { w => tmap.getOrElse(w, 0.0) + w.committed > minWork}
	
		  // Replace workers in current map with updated workers (since all are immutable)
		  // val updatedCurrent = current map { case (j, (w, b)) => (j, (updatedWorkers.find(_ equals w).getOrElse(w), b)) }
		  val updatedCurrent = current.values map { case Bid(w, j, p) => (j, Bid(updatedWorkers.find(_ equals w).getOrElse(w), j, p)) } toMap
		  
		  // Bidding stop when all workers have reached minimum work commitment, or all jobs committed
		  rebidders match {
		    case Nil => (updatedCurrent, updatedWorkers)
		    case _ if (current.size equals js.size) && (current equals offers) => (updatedCurrent, updatedWorkers)
		    case _ => marketBidding( updatedWorkers, rebidders, js, updatedCurrent )
		  }
	  }	  
	  
	  marketBidding( inws, inws, js, Map.empty[Job, Bid])
  }
  
  
  
  // Simulate bidding in increments
  // For each increment, simulate market bidding process
  // Initial bids should be based on worker's historical rates
  
  // Find workers who do not have the minimum commitment level
  // These workers now submit new bids
  // Workers should be able to submit multiple simultaneous bids (eg. a lower bid on the same job, and a new bid on a different job)
  // -- but is it necessary? ie. does this produce the same result? -- test
  
  // Workers need a function to determine successive bids.
  // -- Can we just remember the most recent rate, or 'standard'?
  // -- Evaluate each job to determine its value, bid against highest value
  // -- Actual bid determines the worker pay-off
  // -- If rejected, must increase the bid (which decreases the pay-off) of that job
  // -- Select second best value job. Generate bids with incrementally lower pay-offs for some jobs for which worker can receive excess value

  // Find best allocation with initial bids
  //   Under-allocated staff make new bids
  //   repeat until no new bids are accepted? Or for some number of rounds?
  // Workers start work on their preferred, assigned jobs:
  // Sort and partition these jobs (or partition and repeat) until all workers have consumed minWork jobs.
  // OR assume all scheduled jobs are committed -- each worker enters the next round with their own min-work to fill.
  
  // Overall market process:
  // Generate a batch of jobs, slightly exceeding requirement (workMin * workers)
  // Allocate these jobs using marketBidding
  // Determine unallocated jobs
  // Generate additional jobs to slightly exceed requirement again
  // Determine worker commitment (and rates) from most recent set of allocations
  // Continue market bidding until max work reached
  
  // Think about how to calculate stats:
  // eg. number of rounds of bidding
  // number of bids per job
  // number of bids per worker (to get a job)
  
  // Step through an example in worksheet?
  
}