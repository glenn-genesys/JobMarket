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
 
  val rs = (1 to 100) map { b => normDist(10, 2) }//> rs  : scala.collection.immutable.IndexedSeq[Double] = Vector(11.18522738502
                                                  //| 9869, 9.833306865349698, 11.729832088170397, 11.70714350882205, 7.958909206
                                                  //| 728736, 8.040125861434555, 7.867118074600803, 9.21753851156703, 9.730368147
                                                  //| 220137, 9.90591582641019, 10.605818815293325, 10.053042279791168, 10.331879
                                                  //| 546361083, 9.387952198853796, 10.028209713434883, 10.420930039730099, 10.09
                                                  //| 571887811903, 10.443510193825162, 8.716566246718237, 11.028477203884012, 10
                                                  //| .854161445098075, 9.529865369110782, 12.253163676769297, 11.582959236907122
                                                  //| , 10.93948259460581, 7.604293029177391, 12.220277570687767, 11.291651424639
                                                  //| 731, 11.683957927269553, 9.624068634953941, 12.700183152846655, 11.36182923
                                                  //| 1736067, 11.91986268608877, 10.546467528160079, 9.275155945642192, 8.500768
                                                  //| 003641067, 9.979472263074534, 8.82876511757067, 10.02747115854623, 9.915743
                                                  //| 249712452, 7.695062020435582, 9.928236774707699, 11.03940436610906, 8.81403
                                                  //| 016638356, 9.8989074768
                                                  //| Output exceeds cutoff limit.
  
  rs.sorted                                       //> res0: scala.collection.immutable.IndexedSeq[Double] = Vector(7.604293029177
                                                  //| 391, 7.695062020435582, 7.867118074600803, 7.88964095209821, 7.958909206728
                                                  //| 736, 8.040125861434555, 8.165462259797758, 8.500768003641067, 8.70245007631
                                                  //| 34, 8.716566246718237, 8.772596475015959, 8.783434653696535, 8.814030166383
                                                  //| 56, 8.82876511757067, 8.893230867209496, 9.040569967216449, 9.1878448246165
                                                  //| 17, 9.21753851156703, 9.275155945642192, 9.303371445207105, 9.3097166499446
                                                  //| 67, 9.3772331678198, 9.387952198853796, 9.402904263012088, 9.41654522228873
                                                  //| 6, 9.455665377672092, 9.529865369110782, 9.624068634953941, 9.6704018947692
                                                  //| 2, 9.67122613954028, 9.730368147220137, 9.76273140165923, 9.80215785619198,
                                                  //|  9.833306865349698, 9.854162089139436, 9.898907476842862, 9.90591582641019,
                                                  //|  9.915743249712452, 9.928236774707699, 9.960721011304047, 9.979472263074534
                                                  //| , 10.015323600690987, 10.02747115854623, 10.028209713434883, 10.04675450972
                                                  //| 6732, 10.05304227979116
                                                  //| Output exceeds cutoff limit.
  
  val h = JobSim.histogram(rs, 5)                 //> h  : scala.collection.immutable.IndexedSeq[(Double, Int)] = Vector((7.0,5),
                                                  //|  (8.0,10), (9.0,26), (10.0,32), (11.0,21), (12.0,5), (13.0,1))
  (h.size, h map (_._2) sum)                      //> res1: (Int, Int) = (7,100)
  
  
  1 to 100 map (JobSim.f10ceil(_))                //> res2: scala.collection.immutable.IndexedSeq[Double] = Vector(1.0, 2.0, 5.0,
                                                  //|  5.0, 5.0, 10.0, 10.0, 10.0, 10.0, 10.0, 10.0, 10.0, 10.0, 10.0, 10.0, 10.0
                                                  //| , 10.0, 10.0, 10.0, 20.0, 20.0, 20.0, 20.0, 20.0, 20.0, 20.0, 20.0, 20.0, 2
                                                  //| 0.0, 50.0, 50.0, 50.0, 50.0, 50.0, 50.0, 50.0, 50.0, 50.0, 50.0, 50.0, 50.0
                                                  //| , 50.0, 50.0, 50.0, 50.0, 50.0, 50.0, 50.0, 50.0, 50.0, 50.0, 50.0, 50.0, 5
                                                  //| 0.0, 50.0, 50.0, 50.0, 50.0, 50.0, 100.0, 100.0, 100.0, 100.0, 100.0, 100.0
                                                  //| , 100.0, 100.0, 100.0, 100.0, 100.0, 100.0, 100.0, 100.0, 100.0, 100.0, 100
                                                  //| .0, 100.0, 100.0, 100.0, 100.0, 100.0, 100.0, 100.0, 100.0, 100.0, 100.0, 1
                                                  //| 00.0, 100.0, 100.0, 100.0, 100.0, 100.0, 100.0, 100.0, 100.0, 100.0, 100.0,
                                                  //|  100.0, 100.0, 100.0)
  
  
  (((rs map (_.toInt)) groupBy(identity)).values.map {vs => (vs.head, vs.size)}).toList.sortBy(_._1)
                                                  //> res3: List[(Int, Int)] = List((7,5), (8,10), (9,26), (10,32), (11,21), (12,
                                                  //| 5), (13,1))
  (((rs sorted) map (_.toInt)) groupBy(identity) values) map (_.size)
                                                  //> res4: Iterable[Int] = List(32, 26, 1, 5, 5, 21, 10)
  
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
 x map {case (_, r) => r}                         //> res5: List[Int] = List(2, 4, 6, 8)
 x.unzip._2                                       //> res6: List[Int] = List(2, 4, 6, 8)


  val results = JobSim.comparativeMarketSim( 3, 0.2, 3, 0.5, 2.0, List(CreditMarket,PreferenceMarket) )
                                                  //> results  : scala.collection.immutable.IndexedSeq[Iterable[scala.collection.
                                                  //| mutable.LinkedHashMap[String,Any]]] = Vector(List(Map(Market Type -> JobMar
                                                  //| ket.CreditMarket$, Stable matchings -> List(true), creditMap -> List(Map(1 
                                                  //| -> 0.9616626811495531, 3 -> 1.075137574240605, 2 -> 1.0375438482591461)), C
                                                  //| redit History -> Map(1 -> List(0.9616626811495531), 3 -> List(1.07513757424
                                                  //| 0605), 2 -> List(1.0375438482591461)), Credit Rates -> List(0.9616626811495
                                                  //| 531, 1.075137574240605, 1.0375438482591461), Av credit rate -> 1.025+/-0.05
                                                  //| 8, Production rate -> 1.021+/-0.054, Pay-off ratio (Job/Worker) -> 0.989+/-
                                                  //| 0.021, Num bids -> 16, Num jobs -> 8, Av bids per worker per job -> 2.06+/-
                                                  //| 0.63, Av bids per job -> 2.0, Work allocated per worker -> 0.58579731863581
                                                  //| 26, Total time allocated -> 1.6792769135148007, Total work allocated -> 1.7
                                                  //| 573919559074378, Overall value -> 1.0465170703914097, Time allocated per wo
                                                  //| rker -> 0.5597589711716
                                                  //| Output exceeds cutoff limit.
 

 
  
}