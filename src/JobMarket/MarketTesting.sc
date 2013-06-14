package JobMarket

import util.Random

object MarketTesting {
  def funPrint[T]( x: T ) = { println(x); x }     //> funPrint: [T](x: T)T

  def mean[T](xs:Iterable[T])( implicit num: Numeric[T] ) = num.toDouble(xs.sum) / xs.size
                                                  //> mean: [T](xs: Iterable[T])(implicit num: Numeric[T])Double

  def std[T](xs:Iterable[T])( implicit num: Numeric[T] ) = 	{
    val av = mean(xs)
	  math.sqrt((mean(xs map {v => List(v, v).product}) - av*av)*xs.size/(xs.size - 1))
  }                                               //> std: [T](xs: Iterable[T])(implicit num: Numeric[T])Double
  
  def wmean(xs:Iterable[(Double,Double)]) = (xs.foldLeft((0.0, 0.0))((mw, vw) => (mw._1 + vw._1*vw._2, mw._2 + vw._2))) match {case (v,w) => v/w}
                                                  //> wmean: (xs: Iterable[(Double, Double)])Double

  // def mean(xs: List[Double]) = xs.sum / xs.size

	val (minWork, maxWork) = (0.2, 1.0)       //> minWork  : Double = 0.2
                                                  //| maxWork  : Double = 1.0

	  def marketBidding( ws: List[Worker], js: List[Job], offers: Map[Job, (Worker, Double)] ): Map[Job, (Worker, Double)] = {
	      // Get bids of given workers for given jobs
		  val bids = Market.workerBids(ws, js, offers)
	
	      // Update current best offers
		  val current = Market.considerOffers( bids, { case (j, (w, p)) => j.workerTime(w)/p }, offers )
		  
		  // Bidding stops when no new bids are accepted in a round
		  if (current equals offers) return current
		  
		  val tmap = Market.timeCommitment(current)
		  
		  // Determine the average crediting rate implied by current allocation
		  val cmap = Market.creditRate(current)
		   
		  // Update crediting rate of workers according to whether their bid was rejected or not
		  val updatedWorkers = ws map { w => Worker(w.name, w.efficiency, cmap.getOrElse(w, w.rate * 0.97), tmap.getOrElse(w, 0.0) + w.committed,
		    										(bids collect { case (`w`, (j, b)) => (j, b) }):::w.bids ) }
		  
		  // Partition into those workers that need to rebid, and those that can sit
		  val (sitters, rebidders) = updatedWorkers partition {_.committed > minWork}
	
			current
		  // marketBidding( rebidders, js, current )
	  }                                       //> marketBidding: (ws: List[JobMarket.Worker], js: List[JobMarket.Job], offers
                                                  //| : Map[JobMarket.Job,(JobMarket.Worker, Double)])Map[JobMarket.Job,(JobMarke
                                                  //| t.Worker, Double)]
	  
	def normDist( mean: Double, std: Double ) = {
    (((1 to 64) map ( _ => math.random ) sum) - 32.0 )/ 4.0 * std  + mean
  }                                               //> normDist: (mean: Double, std: Double)Double
  
  val numDisciplines = 5                          //> numDisciplines  : Int = 5
	val numWorkers = 5                        //> numWorkers  : Int = 5
	
	val numJobs = 10                          //> numJobs  : Int = 10
	  
	val workers = (1 to numWorkers) map (i => Worker(i.toString, (1 to numDisciplines) map ( _ => normDist(1.0, 0.5) abs ) toList )) toList
                                                  //> workers  : List[JobMarket.Worker] = List(1, 2, 3, 4, 5)
 
	val jobs = JobSim.orphanJobs( numJobs, numWorkers, 0.3, numDisciplines)
                                                  //> jobs  : List[JobMarket.Job] = List(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)

  println("Workers:")                             //> Workers:
  for (w <- workers) println( w.name + ": " + w.efficiency )
                                                  //> 1: List(0.6458633246855761, 1.6483907991870757, 1.2049749230380762, 1.06550
                                                  //| 60278723525, 1.1463783631060762)
                                                  //| 2: List(1.280490053096977, 1.5922431381225257, 0.7475435576161411, 0.953801
                                                  //| 2246729783, 0.8349298397854392)
                                                  //| 3: List(0.9313471236737918, 0.8249746254337325, 0.8892378324324475, 1.16406
                                                  //| 61405062875, 0.9808928539607775)
                                                  //| 4: List(1.0876551669441472, 0.8987941708979332, 1.1202549011230882, 0.99948
                                                  //| 62864885752, 0.32544111962904365)
                                                  //| 5: List(0.8035761742914658, 0.6808520730873369, 0.9957333638611274, 0.95318
                                                  //| 84954758918, 0.7996898178132743)
  
  println("Jobs:")                                //> Jobs:
  for (j <- jobs) println( j.id + ": " + j.skills )
                                                  //> 1: List(0.00865746039986444, 0.010013806973393614, 0.1496407872478769, 0.26
                                                  //| 957434675410613, 0.07715465051235164)
                                                  //| 2: List(0.024899700886001266, 0.004675498336553934, 0.01542626351645838, 0.
                                                  //| 013759398905124938, 0.022573441793935053)
                                                  //| 3: List(0.009767025329480901, 0.0110657781254339, 0.20217859676389793, 0.02
                                                  //| 2613248759010423, 0.07807503922208436)
                                                  //| 4: List(0.0075819820511541115, 0.01171939902051328, 0.5190538711254536, 0.0
                                                  //| 03140626125931822, 0.0031165424351339726)
                                                  //| 5: List(0.00568474637030579, 0.373579827135279, 0.19985408588796127, 0.0100
                                                  //| 9259024068486, 0.12036862838279869)
                                                  //| 6: List(0.10384749397146895, 0.23829425578994645, 0.26824766877005574, 0.00
                                                  //| 5798758093745943, 0.0866274730951804)
                                                  //| 7: List(0.008498984220369757, 0.004677132199412353, 0.34753727209209295, 0.
                                                  //| 004601841607114761, 0.11982643508495792)
                                                  //| 8: List(0.007315625232250128, 0.03618195420257258, 0.021412762438478086, 0.
                                                  //| 3009012212014727, 0.159159083
                                                  //| Output exceeds cutoff limit.

	val jobPrefs = workers.map( w => (w, w.jobPrefs(jobs)) )
                                                  //> jobPrefs  : List[(JobMarket.Worker, List[JobMarket.Job])] = List((1,List(5,
                                                  //|  4, 7, 3, 6, 8, 1, 9, 10, 2)), (2,List(5, 6, 2, 9, 10, 8, 1, 3, 7, 4)), (3,
                                                  //| List(9, 10, 8, 1, 2, 3, 7, 4, 6, 5)), (4,List(4, 9, 10, 6, 1, 5, 3, 7, 2, 8
                                                  //| )), (5,List(4, 9, 7, 1, 3, 10, 8, 2, 6, 5)))
	  
	val jobValues = jobPrefs map { case (w, j::_) => j -> j.workload/j.workerTime(w) }
                                                  //> jobValues  : List[(JobMarket.Job, Double)] = List((5,1.3757855301618964), (
                                                  //| 5,1.0732351953817705), (9,1.1605670133100874), (4,1.0978670829217354), (4,0
                                                  //| .9810747537023611))
	  
  val allBids = workers.map( w => (w, jobs map {j => (j, (w.bids.collectFirst { case (`j`, b) => b*0.95 }).getOrElse((j.workload + j.workerTime(w)*w.rate)/2.0) ) } ) )
                                                  //> allBids  : List[(JobMarket.Worker, List[(JobMarket.Job, Double)])] = List((
                                                  //| 1,List((1,0.48950521650381024), (2,0.08406500738895026), (3,0.3013252974800
                                                  //| 9207), (4,0.49994326312801785), (5,0.6126716591097319), (6,0.65589566440227
                                                  //| 84), (7,0.4492009226366399), (8,0.4986281553658013), (9,0.5706226330456918)
                                                  //| , (10,0.6670946678255959))), (2,List((1,0.551654013215037), (2,0.0829071505
                                                  //| 9914144), (3,0.3629768965929073), (4,0.6296326604613757), (5,0.685369797491
                                                  //| 7331), (6,0.7011237957668722), (7,0.55398181455524), (8,0.5440766803272468)
                                                  //| , (9,0.6009770287296876), (10,0.6969663781103145))), (3,List((1,0.507496162
                                                  //| 8521242), (2,0.08295895287249273), (3,0.3369918977895372), (4,0.57827038027
                                                  //| 65676), (5,0.7623263473482877), (6,0.749062554381292), (7,0.508438197722444
                                                  //| 4), (8,0.5107573063666055), (9,0.5470127841699498), (10,0.653297174962685))
                                                  //| ), (4,List((1,0.5872548337558194), (2,0.10316433336685693), (3,0.3939985270
                                                  //| 015378), (4,0.520338248
                                                  //| Output exceeds cutoff limit.
  
  allBids map { case (w, jbs) => (w, jbs maxBy { case (j, b) => b/j.workerTime(w) } ) }
                                                  //> res0: List[(JobMarket.Worker, (JobMarket.Job, Double))] = List((1,(5,0.6126
                                                  //| 716591097319)), (2,(5,0.6853697974917331)), (3,(9,0.5470127841699498)), (4,
                                                  //| (4,0.5203382486968917)), (5,(4,0.5498652947928645)))
 
	val m = new Market(numDisciplines)        //> m  : JobMarket.Market = JobMarket.Market@1a46db0d

	val mc = m.deferredAcceptance( workers, jobs )
                                                  //> mc  : scala.collection.immutable.Map[JobMarket.Job,JobMarket.Worker] = Map(
                                                  //| 6 -> 2, 4 -> 4, 3 -> 1, 9 -> 3, 7 -> 5, 2 -> 2, 5 -> 1)
   
  marketBidding( workers, jobs, Map.empty[Job, (Worker, Double)])
                                                  //> res1: Map[JobMarket.Job,(JobMarket.Worker, Double)] = Map(5 -> (2,0.6853697
                                                  //| 974917331), 9 -> (3,0.5470127841699498), 4 -> (5,0.5498652947928645))
  
	val (mb, ww) = m.marketBidding( workers, jobs, 0.2 )
                                                  //> mb  : scala.collection.immutable.Map[JobMarket.Job,(JobMarket.Worker, Doubl
                                                  //| e)] = Map(6 -> (2,0.7011237957668722), 4 -> (4,0.5203382486968917), 9 -> (3
                                                  //| ,0.5470127841699498), 7 -> (5,0.5031415332889059), 5 -> (1,0.61267165910973
                                                  //| 19))
                                                  //| ww  : List[JobMarket.Worker] = List(1, 2, 3, 4, 5)
	
	(jobs.toSet &~ mb.keySet).toList.sortBy(_.id)
                                                  //> res2: List[JobMarket.Job] = List(1, 2, 3, 8, 10)
	(mb groupBy { _._2._1 } keySet)           //> res3: scala.collection.immutable.Set[JobMarket.Worker] = Set(4, 5, 1, 2, 3)
                                                  //| 
	workers.toSet &~ (mb groupBy { _._2._1 } keySet)
                                                  //> res4: scala.collection.immutable.Set[JobMarket.Worker] = Set()
}