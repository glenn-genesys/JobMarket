package JobMarket

trait MarketType {
  def bidfun: (Worker, Job) => Option[Bid]
  
  def selectfun: Bid => Double = _.productionRate
  
  override def toString = getClass().getName()
}


/**
 * Defines the bidding function that workers use: 
 * given a given worker and a job, return a new bid for that job or None
 * @author burgessg
 * @param split A ratio to split the payoff between worker and job, for an initial bid. eg. 0.7 is 70:30 in favour of the worker
 * @param rebid A fraction to multiply by a previous bid, to rebid. eg. 0.95 means next bid is 5% lower than previous
 * @param maxRateDrop In a single batch, the minimum fraction of the worker's previous rate they will consider dropping to 
 * (before they would rather be unemployed -- to prevent jobs being bid down to zero). 
 * eg. 0.3 means they will drop to 0.3 of their rate in the previous batch
 */
case class CreditMarket( split: Double = 0.5, rebid: Double = 0.95, maxRateDrop: Double = 0.5 ) extends MarketType {
  def bidfun: (Worker, Job) => Option[Bid] = (w, j) => {
    // If there was a previous bid for this job, beat that bid by 5%. 
    val prev = (w.bids.collectFirst { case Bid(_,`j`,p) => p*rebid })
    // Otherwise bid to split the payoff 50/50
    val bid = Bid(w, j, prev.getOrElse(j.workload*(1-split) + j.workerTime(w)*w.rate*split) )

    // But minimum bid is half of initial credit rate -- to stop two workers infinitely bidding a price down to zero
    if (bid.creditRate < w.rate*maxRateDrop) None
    else Some(bid)
	// (w, j) => Some(Bid(w, j, (w.bids.collectFirst { case Bid(_,`j`,p) => p*0.95 }).getOrElse((j.workload + j.workerTime(w)*w.rate)/2.0) ))
  }
}

object PreferenceMarket extends MarketType {
    def bidfun: (Worker, Job) => Option[Bid] = {
	    case (w, j) if w.bids.exists( _.job equals j) => None
	    // case (w, j) => Some(Bid(w, j, j.workload))
	    // Worker's next preference is that which has the highest value (work/time), 
	    // but average work and time makes worker prefs and job prefs correct
	    case (w, j) => Some(Bid(w, j, (j.workload + j.workerTime(w))/2.0))
	  }
}

object RandomMarket extends MarketType {
  // In a random market, worker preferences and job preferences are random, but fixed 
    def bidfun: (Worker, Job) => Option[Bid] = {
	    case (w, j) if w.bids.exists( _.job equals j) => None
	    case (w, j) => Some(Bid(w, j, j.workerTime(w).hashCode))
	  }

    override def selectfun: Bid => Double = (_.hashCode)
}



	      
	  
	  
