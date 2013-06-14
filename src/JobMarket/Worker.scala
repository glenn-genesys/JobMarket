package JobMarket

/**
 * @author burgessg
 * 
 * A single worker in a job market. Each worker is immutable for each job.
 * @param name A unique identifier for each worker
 * @param efficiency An ordered list. The rate of production of this worker for each corresponding skill/discipline area
 * @param rate Typical crediting rate
 */
case class Worker(name: String, efficiency: List[Double], rate: Double = 1.0, committed: Double = 0.0, bids: List[(Job, Double)] = Nil) {
  
  /**
   * Return the list of jobs, in preferred order (most preferred first)
   * Workers prefer the jobs that they can do most efficiently
   */
  def jobPrefs( js: List[Job] ) = js.sortBy( j => j.workerTime(this)/j.skills.sum )

  override def toString = name //  + ": " + efficiency.toString
  
  override def equals(o: Any) = o match {
    case w: Worker => w.name == name
    case _ => false
  }
  
  override def hashCode = name.hashCode
}