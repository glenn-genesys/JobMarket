package JobMarket

trait MarketType {
  def bidfun: (Worker, Job) => Option[Bid]
  
  def selectfun: Bid => Double = _.productionRate
}

object CreditMarket extends MarketType {
  def bidfun: (Worker, Job) => Option[Bid] = 
	(w, j) => Some(Bid(w, j, (w.bids.collectFirst { case Bid(_,`j`,p) => p*0.95 }).getOrElse((j.workload + j.workerTime(w)*w.rate)/2.0) ))
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
  // In a random market, worker preferences are random and jobs accept the first bid they get 
    def bidfun: (Worker, Job) => Option[Bid] = {
	    case (w, j) if w.bids.exists( _.job equals j) => None
	    case (w, j) => Some(Bid(w, j, j.workerTime(w).hashCode))
	  }

    override def selectfun: Bid => Double = (_.hashCode)
}



	      
	  
	  
