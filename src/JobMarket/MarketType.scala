package JobMarket

trait MarketType {
  def bidfun: (Worker, Job) => Option[Bid]
  
  def selectfun: Bid => Double = _.productionRate
}

object CreditMarket extends MarketType {
  def bidfun: (Worker, Job) => Option[Bid] = (w, j) => {
    // If there was a previous bid for this job, beat that bid by 5%. 
    val prev = (w.bids.collectFirst { case Bid(_,`j`,p) => p*0.95 })
    // Otherwise bid to split the payoff 50/50
    val bid = Bid(w, j, prev.getOrElse((j.workload + j.workerTime(w)*w.rate)/2.0) )
    
    // But minimum bid is half of initial credit rate -- to stop two workers infinitely bidding a price down to zero
    if (bid.creditRate < w.rate/2) None
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



	      
	  
	  
