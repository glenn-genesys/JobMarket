package JobMarket

import JobSim._
import scala.collection.mutable.LinkedHashMap

class MarketSimulation(numWorkers: Int, jobSize: Double, numDisciplines: Int, simYears: Double, batchFreq: Double, mType: MarketType, margin: Double, paramType: String, p: Any) {
    // Overall market process:
  	// Generate a batch of jobs, slightly exceeding requirement (workMin * workers)
  	// Allocate these jobs using marketBidding
  	// Determine unallocated jobs
  	// Generate additional jobs to slightly exceed requirement again
    // Determine worker commitment (and rates) from most recent set of allocations
    // Continue market bidding until max work reached
    def marketSim( m: Market, ws: List[Worker], js: Iterable[Job], moreJobs: Stream[Job], minWork: Double, simYears: Double, mType: MarketType,
                   acc: List[(List[Worker], List[Job], List[Bid])] ): List[(List[Worker], List[Job], List[Bid])] = {
      val (matching, workers0) = m.marketBidding(ws, js, minWork, mType)
	    
      // Update workers with current commitment, to a minimum of minWork, since that time has passed
      val tmap = Market.timeCommitment(matching)
	  val updatedWorkers = workers0 map { w => Worker(w.name, w.efficiency, w.rate, math.max(tmap.getOrElse(w, 0.0) + w.committed, minWork)) }

      val unfilledJobs = (js.toSet &~ matching.map(_.job).toSet) map (_.nextRound) toList
      val newWork = m.numWorkers*m.workStep*m.margin - unfilledJobs.map(_.workload).sum
      val numJobs = newWork/m.jobSize toInt
      
      val (newJobs, nextJobs) = moreJobs.splitAt(numJobs)
      
      if (minWork < simYears)
        marketSim( m, updatedWorkers, unfilledJobs:::newJobs.toList, nextJobs, minWork + m.workStep, simYears, mType, (updatedWorkers, unfilledJobs, matching)::acc )
      else
    	(updatedWorkers, unfilledJobs, matching)::acc
    }
    
    val results = {
    	val market = new Market(mType, numWorkers, numDisciplines, workStep = 1.0/batchFreq, margin, jobSize)

    	// val batchWork = numWorkers/batchFreq*margin
    	val firstJobs = numWorkers/batchFreq*margin/jobSize toInt

    	// Create a new sample of workers and jobs for each run
    	val workers = (1 to numWorkers) map (i => Worker(i.toString, (1 to numDisciplines) map ( _ => 1.0/normRand(1.0, 0.2) abs ) toList )) toList

    	// Create stream of jobs to consume, reused for all market types
    	val allJobs = jobStream( orphanJob(jobSize, jobSize/2.0, numDisciplines ) )
    	val (newJobs, nextJobs) = allJobs.splitAt(firstJobs)

    	val simParams = LinkedHashMap[String, Any]("Num Workers" -> numWorkers, 
    	    "Job Size" -> jobSize, 
    	    "Num Disciplines" -> numDisciplines,
    	    "Sim years" -> simYears,
    	    "Batch frequency" -> batchFreq,
    	    "Market type" -> mType,
    	    "Job workload margin" -> margin,
    	    "Varied parameter: " + paramType -> p)
    	
    	collectResults(marketSim( market, workers, newJobs, nextJobs, market.workStep, simYears, mType, Nil ).reverse, simParams)
    }

    
}