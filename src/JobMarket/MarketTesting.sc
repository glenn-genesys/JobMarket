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
                                                  //> 1: List(1.2285809289378307, 1.0726563090915828, 1.1996362282478827, 0.65686
                                                  //| 96207256521, 0.7851625338537422)
                                                  //| 2: List(0.9687780013110707, 1.0104360747915964, 1.7563138538066223, 1.09604
                                                  //| 54931353967, 0.9627221450830996)
                                                  //| 3: List(1.3328919948711055, 1.3515602811260798, 0.9571892586270803, 0.88169
                                                  //| 81983055463, 1.1504634654764052)
                                                  //| 4: List(0.7073183726890999, 0.7895532933489471, 0.6779156747556474, 0.82160
                                                  //| 99617348416, 0.5882975891875293)
                                                  //| 5: List(1.0140971106125676, 1.05358404164451, 1.4547909494605396, 0.7592074
                                                  //| 453299213, 0.6767801436357757)
  
  println("Jobs:")                                //> Jobs:
  for (j <- jobs) println( j.id + ": " + j.skills )
                                                  //> 1: List(0.08079441036491586, 0.05723241360861297, 0.04306080898352331, 0.00
                                                  //| 9302882586187666, 0.05478355179949081)
                                                  //| 2: List(0.015988614134722735, 0.04087292697101116, 0.21933472436347273, 0.0
                                                  //| 010119837632525583, 0.005757413432158795)
                                                  //| 3: List(0.03912941776075169, 0.02207341735318325, 0.45088149774725395, 0.04
                                                  //| 419582164068634, 0.020095605759048854)
                                                  //| 4: List(0.11717549227233569, 0.007300340347091257, 0.15994382713128424, 0.0
                                                  //| 7775391782805767, 0.008364571551099615)
                                                  //| 5: List(0.11321304138789612, 0.19624978326331582, 0.011465912056594394, 0.0
                                                  //| 7059487819090167, 0.15307428692285752)
                                                  //| 6: List(0.0940115460636038, 0.1553948380952209, 0.2870202702406902, 0.06587
                                                  //| 782316077141, 0.028851265801269243)
                                                  //| 7: List(0.12399244656089012, 0.17717164569598964, 0.3495332716388028, 0.027
                                                  //| 02233085683489, 0.030514576433766625)
                                                  //| 8: List(0.5173087662880228, 0.00437322790722493, 0.0649692507817248, 0.0272
                                                  //| 8664714175879, 0.21599599664414046)

	val jobPrefs = workers.map( w => (w, w.jobPrefs(jobs)) )
                                                  //> jobPrefs  : List[(JobMarket.Worker, List[JobMarket.Job])] = List((1,List(2,
                                                  //|  7, 3, 10, 6, 8, 1, 4, 9, 5)), (2,List(3, 2, 10, 7, 6, 4, 1, 5, 8, 9)), (3,
                                                  //| List(9, 8, 5, 1, 7, 6, 10, 4, 2, 3)), (4,List(6, 4, 7, 5, 10, 2, 1, 3, 8, 9
                                                  //| )), (5,List(2, 3, 7, 10, 6, 4, 1, 8, 5, 9)))
	  
	val jobValues = jobPrefs map { case (w, j::_) => j -> j.workload/j.workerTime(w) }
                                                  //> jobValues  : List[(JobMarket.Job, Double)] = List((2,1.1653020002645291), (
                                                  //| 3,1.5161679163731703), (9,1.2415761886521042), (6,0.715323594598565), (2,1.
                                                  //| 3150934487767065))
	  
  val allBids = workers.map( w => (w, jobs map {j => (j, (w.bids.collectFirst { case (`j`, b) => b*0.95 }).getOrElse((j.workload + j.workerTime(w)*w.rate)/2.0) ) } ) )
                                                  //> allBids  : List[(JobMarket.Worker, List[(JobMarket.Job, Double)])] = List((
                                                  //| 1,List((1,0.24206154298469407), (2,0.2628958481298359), (3,0.54876425929853
                                                  //| 71), (4,0.3675346503827297), (5,0.5658461898930647), (6,0.6144188007312219)
                                                  //| , (7,0.6728483153386444), (8,0.8129339915498182), (9,0.23373865994150722), 
                                                  //| (10,0.45747906156112017))), (2,List((1,0.23756193967849945), (2,0.235853776
                                                  //| 18928505), (3,0.4782643729900638), (4,0.3347058586099659), (5,0.54281054405
                                                  //| 5867), (6,0.5677411965184603), (7,0.6334651683661868), (8,0.827244834086042
                                                  //| 7), (9,0.2343666541967972), (10,0.41307950689970113))), (3,List((1,0.225645
                                                  //| 93938287253), (2,0.28024957693913755), (3,0.5803524618193623), (4,0.3632024
                                                  //| 147379466), (5,0.499919156335085), (6,0.608157233653196), (7,0.677342118017
                                                  //| 3813), (8,0.7539247107765925), (9,0.20884775868560299), (10,0.4585112800795
                                                  //| 549))), (4,List((1,0.29992593043670585), (2,0.34594921285723923), (3,0.7063
                                                  //| 51862190945), (4,0.4451
                                                  //| Output exceeds cutoff limit.
  
  allBids map { case (w, jbs) => (w, jbs maxBy { case (j, b) => b/j.workerTime(w) } ) }
                                                  //> res0: List[(JobMarket.Worker, (JobMarket.Job, Double))] = List((1,(2,0.2628
                                                  //| 958481298359)), (2,(3,0.4782643729900638)), (3,(9,0.20884775868560299)), (4
                                                  //| ,(6,0.7567458606353125)), (5,(2,0.24906669274072496)))
 
	val m = new Market(numDisciplines)        //> m  : JobMarket.Market = JobMarket.Market@518f5824

	val mc = m.deferredAcceptance( workers, jobs )
                                                  //> mc  : scala.collection.immutable.Map[JobMarket.Job,JobMarket.Worker] = Map(
                                                  //| 9 -> 3, 1 -> 3, 2 -> 2, 6 -> 4, 10 -> 1, 3 -> 2, 7 -> 5, 5 -> 3)
   
  marketBidding( workers, jobs, Map.empty[Job, (Worker, Double)])
                                                  //> res1: Map[JobMarket.Job,(JobMarket.Worker, Double)] = Map(3 -> (2,0.4782643
                                                  //| 729900638), 2 -> (1,0.2628958481298359), 9 -> (3,0.20884775868560299), 6 ->
                                                  //|  (4,0.7567458606353125))
  
	val (mb, ww) = m.marketBidding( workers, jobs, 0.2 )
                                                  //> mb  : scala.collection.immutable.Map[JobMarket.Job,(JobMarket.Worker, Doubl
                                                  //| e)] = Map(9 -> (3,0.20884775868560299), 2 -> (5,0.24906669274072496), 6 -> 
                                                  //| (4,0.7567458606353125), 8 -> (3,0.7948667734364786), 3 -> (2,0.478264372990
                                                  //| 0638), 7 -> (1,0.6728483153386444))
                                                  //| ww  : List[JobMarket.Worker] = List(1, 2, 3, 4, 5)
	
	(jobs.toSet &~ mb.keySet).toList.sortBy(_.id)
                                                  //> res2: List[JobMarket.Job] = List(1, 4, 5, 10)
	(mb groupBy { _._2._1 } keySet)           //> res3: scala.collection.immutable.Set[JobMarket.Worker] = Set(4, 5, 1, 2, 3)
                                                  //| 
	workers.toSet &~ (mb groupBy { _._2._1 } keySet)
                                                  //> res4: scala.collection.immutable.Set[JobMarket.Worker] = Set()
}