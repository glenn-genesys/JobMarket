package JobMarket

import util.Random
import JobSim._

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
 
 
	def normDist( mean: Double, std: Double ) = {
    (((1 to 64) map ( _ => math.random ) sum) - 32.0 )/ 4.0 * std  + mean
  }                                               //> normDist: (mean: Double, std: Double)Double

  val numWorkers = 100                            //> numWorkers  : Int = 100
  val numDisciplines = 10                         //> numDisciplines  : Int = 10
  val numJobs = 100                               //> numJobs  : Int = 100
  val jobSize = 0.5                               //> jobSize  : Double = 0.5
  
  def histogram[T](xs: Iterable[T], bins: Int)( implicit num: Numeric[T] ) = {
    val (lb, ub) = (xs.min, xs.max)
    val (lx, ux) = (num.toDouble(lb), num.toDouble(ub))
    val rx = bins/ux
    ((xs map { x => ((num.toDouble(x) - lx)*rx) toInt } groupBy(identity)).values.map {vs => (vs.head/rx + lx, vs.size)}).toList.sortBy(_._1)
  }                                               //> histogram: [T](xs: Iterable[T], bins: Int)(implicit num: Numeric[T])List[(D
                                                  //| ouble, Int)]
 5 to 50 by 5 toList                              //> res0: List[Int] = List(5, 10, 15, 20, 25, 30, 35, 40, 45, 50)
  val rs = (1 to 100) map { b => normDist(10, 2) }//> rs  : scala.collection.immutable.IndexedSeq[Double] = Vector(10.64611209260
                                                  //| 6486, 9.033278976384594, 10.402402862698477, 9.645592573609502, 9.680347671
                                                  //| 925016, 9.949558341988995, 9.421505307725683, 10.089860108311054, 8.4736847
                                                  //| 33094377, 11.117431822763887, 8.590944691698532, 7.989457223659823, 7.43359
                                                  //| 4043155233, 9.569370142909102, 9.56819153206323, 9.126854587976275, 9.46754
                                                  //| 6167982542, 9.837844145833742, 9.010716711344413, 9.284522832327955, 10.983
                                                  //| 011986375669, 9.455324384374252, 11.075990836395256, 12.244932751695185, 9.
                                                  //| 887462138715406, 8.954281694489158, 9.421484707784026, 8.790555029299588, 1
                                                  //| 0.088367295184831, 9.367186681168981, 10.704668865308665, 10.20386037816510
                                                  //| 7, 10.493198163219954, 9.226510046261794, 11.583014094636258, 10.7673194815
                                                  //| 13361, 9.509425509861442, 9.564722506516825, 9.713991181433926, 10.76340300
                                                  //| 7664806, 9.044519953138309, 11.91429478613389, 11.25507073255482, 9.4745816
                                                  //| 99445693, 9.50595226754
                                                  //| Output exceeds cutoff limit.
  
  rs.sorted                                       //> res1: scala.collection.immutable.IndexedSeq[Double] = Vector(7.203036261434
                                                  //| 271, 7.433594043155233, 7.623480276597885, 7.932059518243051, 7.98945722365
                                                  //| 9823, 8.062064459415462, 8.068643966480174, 8.116827136376399, 8.4069145737
                                                  //| 72736, 8.473684733094377, 8.516310682267116, 8.560157386769445, 8.590944691
                                                  //| 698532, 8.643377293156028, 8.672107424541016, 8.766647256916714, 8.77740158
                                                  //| 8494632, 8.790555029299588, 8.795958564055221, 8.801193100903287, 8.8650575
                                                  //| 14320174, 8.954281694489158, 9.010716711344413, 9.033278976384594, 9.044519
                                                  //| 953138309, 9.10548478004106, 9.126854587976275, 9.197420072411488, 9.212411
                                                  //| 001581549, 9.226510046261794, 9.257600955691734, 9.284522832327955, 9.31773
                                                  //| 4623564291, 9.36372346661082, 9.367186681168981, 9.421484707784026, 9.42150
                                                  //| 5307725683, 9.455324384374252, 9.467546167982542, 9.474581699445693, 9.5059
                                                  //| 52267541808, 9.509425509861442, 9.522983251101758, 9.546000043308585, 9.564
                                                  //| 722506516825, 9.5681915
                                                  //| Output exceeds cutoff limit.
  
  val h = Histogram(rs, minBins = 5)              //> h  : JobMarket.Histogram = 7.00:ooooo
                                                  //| 8.00:ooooooooooooooooo
                                                  //| 9.00:ooooooooooooooooooooooooooooooooooooooo
                                                  //| 10.00:oooooooooooooooooooooo
                                                  //| 11.00:oooooooooooooo
                                                  //| 12.00:oo
                                                  //| 13.00:o
  (h.numBins, h.data sum)                         //> res2: (Int, Double) = (7,100.0)
  
  
  1 to 100 map (Histogram.f10ceil(_))             //> res3: scala.collection.immutable.IndexedSeq[Double] = Vector(1.0, 2.0, 5.0,
                                                  //|  5.0, 5.0, 10.0, 10.0, 10.0, 10.0, 10.0, 20.0, 20.0, 20.0, 20.0, 20.0, 20.0
                                                  //| , 20.0, 20.0, 20.0, 20.0, 50.0, 50.0, 50.0, 50.0, 50.0, 50.0, 50.0, 50.0, 5
                                                  //| 0.0, 50.0, 50.0, 50.0, 50.0, 50.0, 50.0, 50.0, 50.0, 50.0, 50.0, 50.0, 50.0
                                                  //| , 50.0, 50.0, 50.0, 50.0, 50.0, 50.0, 50.0, 50.0, 50.0, 100.0, 100.0, 100.0
                                                  //| , 100.0, 100.0, 100.0, 100.0, 100.0, 100.0, 100.0, 100.0, 100.0, 100.0, 100
                                                  //| .0, 100.0, 100.0, 100.0, 100.0, 100.0, 100.0, 100.0, 100.0, 100.0, 100.0, 1
                                                  //| 00.0, 100.0, 100.0, 100.0, 100.0, 100.0, 100.0, 100.0, 100.0, 100.0, 100.0,
                                                  //|  100.0, 100.0, 100.0, 100.0, 100.0, 100.0, 100.0, 100.0, 100.0, 100.0, 100.
                                                  //| 0, 100.0, 100.0, 100.0, 100.0)
  
  
  (((rs map (_.toInt)) groupBy(identity)).values.map {vs => (vs.head, vs.size)}).toList.sortBy(_._1)
                                                  //> res4: List[(Int, Int)] = List((7,5), (8,17), (9,39), (10,22), (11,14), (12,
                                                  //| 2), (13,1))
  (((rs sorted) map (_.toInt)) groupBy(identity) values) map (_.size)
                                                  //> res5: Iterable[Int] = List(22, 39, 1, 2, 5, 14, 17)
  
  val workers = (1 to numWorkers) map (i => Worker(i.toString, (1 to numDisciplines) map ( _ => 1.0/normRand(1.0, 0.2) abs ) toList )) toList
                                                  //> workers  : List[JobMarket.Worker] = List(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11,
                                                  //|  12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30
                                                  //| , 31, 32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 4
                                                  //| 9, 50, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60, 61, 62, 63, 64, 65, 66, 67, 
                                                  //| 68, 69, 70, 71, 72, 73, 74, 75, 76, 77, 78, 79, 80, 81, 82, 83, 84, 85, 86,
                                                  //|  87, 88, 89, 90, 91, 92, 93, 94, 95, 96, 97, 98, 99, 100)
  
  val allJobs = jobStream( orphanJob(jobSize, jobSize/2.0, numDisciplines ) )
                                                  //> allJobs  : Stream[JobMarket.Job] = Stream(0, ?)
  val jobs = (allJobs take numJobs force) toList  //> jobs  : List[JobMarket.Job] = List(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12
                                                  //| , 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 3
                                                  //| 1, 32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 
                                                  //| 50, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60, 61, 62, 63, 64, 65, 66, 67, 68,
                                                  //|  69, 70, 71, 72, 73, 74, 75, 76, 77, 78, 79, 80, 81, 82, 83, 84, 85, 86, 87
                                                  //| , 88, 89, 90, 91, 92, 93, 94, 95, 96, 97, 98, 99)
  
 val x = List((1,2), (3,4), (5,6), (7,8))         //> x  : List[(Int, Int)] = List((1,2), (3,4), (5,6), (7,8))
 x map {case (_, r) => r}                         //> res6: List[Int] = List(2, 4, 6, 8)
 x.unzip._2                                       //> res7: List[Int] = List(2, 4, 6, 8)


 1 to numDisciplines                              //> res8: scala.collection.immutable.Range.Inclusive = Range(1, 2, 3, 4, 5, 6, 
                                                  //| 7, 8, 9, 10)
 workers(0)                                       //> res9: JobMarket.Worker = 1
 
 0 until numDisciplines map (workers(0).efficiency(_))
                                                  //> res10: scala.collection.immutable.IndexedSeq[Double] = Vector(0.83825846633
                                                  //| 02427, 1.2266231795560747, 0.8862722376699719, 0.8537402373394212, 1.152870
                                                  //| 2047482529, 0.9128530183725647, 0.8470215280207035, 0.8788174772502844, 1.0
                                                  //| 258885725085423, 1.0990157041995894)
}