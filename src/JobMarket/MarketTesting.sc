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
  val rs = (1 to 100) map { b => normDist(10, 2) }//> rs  : scala.collection.immutable.IndexedSeq[Double] = Vector(11.40047954422
                                                  //| 0019, 10.340830025187795, 8.7886304108102, 9.802264627990459, 9.41081969594
                                                  //| 818, 9.614085837978086, 8.882556392043046, 8.657427721367098, 10.8505995348
                                                  //| 10407, 10.344674350427507, 8.492958106414829, 11.61945972897712, 10.0188912
                                                  //| 44086067, 9.956667844535597, 8.168907927194718, 10.362432839687802, 10.7147
                                                  //| 67133714368, 11.374138587400733, 9.459478019035588, 9.56041931687226, 9.737
                                                  //| 299573239172, 9.021158957880019, 9.913740792459514, 9.38749146860704, 9.385
                                                  //| 560708686935, 8.821801021906154, 8.580768337613588, 9.017721565715823, 8.95
                                                  //| 8622820194536, 9.200137188138537, 11.00963304266812, 9.625181879022286, 10.
                                                  //| 255558143195419, 8.080239664070934, 8.749407109528915, 10.883944604844295, 
                                                  //| 9.075439091936904, 10.386180686999378, 9.849162963619193, 8.968082014005516
                                                  //| , 9.717337730489069, 11.042032658132715, 11.857972590777916, 8.551666503989
                                                  //| 487, 9.480326435846345,
                                                  //| Output exceeds cutoff limit.
  
  rs.sorted                                       //> res1: scala.collection.immutable.IndexedSeq[Double] = Vector(7.662612228853
                                                  //| 0105, 7.724627252393111, 7.789606818802939, 7.832150222570231, 7.9124843642
                                                  //| 319345, 7.986555642359159, 8.080239664070934, 8.13518818950405, 8.168907927
                                                  //| 194718, 8.44160995091615, 8.487105755571138, 8.492958106414829, 8.551666503
                                                  //| 989487, 8.579150270312027, 8.580768337613588, 8.599758133427958, 8.64320130
                                                  //| 8319513, 8.657427721367098, 8.749407109528915, 8.77431058361703, 8.78863041
                                                  //| 08102, 8.808770174256543, 8.821801021906154, 8.882556392043046, 8.958622820
                                                  //| 194536, 8.968082014005516, 9.017721565715823, 9.021158957880019, 9.03982446
                                                  //| 6760106, 9.065863145172102, 9.075439091936904, 9.152993747117861, 9.2001371
                                                  //| 88138537, 9.290821024232036, 9.329447791202226, 9.349196916648157, 9.385560
                                                  //| 708686935, 9.38749146860704, 9.399814081355197, 9.41081969594818, 9.4594780
                                                  //| 19035588, 9.480326435846345, 9.524286054367497, 9.553822789884673, 9.560419
                                                  //| 31687226, 9.61408583797
                                                  //| Output exceeds cutoff limit.
  
  val h = Histogram(rs, minBins = 5)              //> h  : JobMarket.Histogram = 15.00:oooooo
                                                  //| 15.50:oooooo
                                                  //| 16.00:oooooooooooooo
                                                  //| 16.50:oooooooooooooooo
                                                  //| 17.00:ooooooooooooooooo
                                                  //| 17.50:oooooooooooooooooo
                                                  //| 18.00:ooooooo
                                                  //| 18.50:ooooooooooo
                                                  //| 19.00:oooo
                                                  //| 19.50:o
  (h.numBins, h.data sum)                         //> res2: (Int, Double) = (10,100.0)
  
  
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
                                                  //> res4: List[(Int, Int)] = List((7,6), (8,20), (9,33), (10,25), (11,15), (12,
                                                  //| 1))
  (((rs sorted) map (_.toInt)) groupBy(identity) values) map (_.size)
                                                  //> res5: Iterable[Int] = List(25, 33, 1, 6, 15, 20)
  
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



 
  
}