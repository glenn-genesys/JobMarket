package JobMarket

/**
 * @author burgessg
 * 
 * A single task
 *
 * @param parent The parent job, if this is a subtask
 * @param skills An ordered list. The amount of work required from each skill/discipline area
 */
case class Job( id: Int, parent: Option[Job], skills: List[Double] ) {
  
  /**
   * Determine the time given worker would take to perform the job
   * Assumed to be the sum of the time for each skill area
   */
  def workerTime( w: Worker ) = {
	  skills.zip(w.efficiency).map( x => x._1 / x._2) sum
  }
	  
  /**
   * Determine the degree of preference for the given worker
   * The preference is highest for the worker who is most efficient at the 
   * required amount of work for each skill. ie. who can do all the work in the least time
   */
  def workerPref( w: Worker ) = 1.0 / workerTime(w)

  /**
   * Return the list of workers, in preferred order (most preferred first)
   */
  def workerPrefs( ws: List[Worker] ) = ws.sortBy( workerTime(_) )

  override def toString = id toString
  
  def workload = skills.sum

}