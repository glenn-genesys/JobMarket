package JobMarket

object Histogram {
  
  /** Round the given number upwards to the nearest factor of 10. ie. {1, 2, 5, 10, 20, 50, 100... } */
  def f10ceil[T]( x: T )( implicit num: Numeric[T] ) = {
    import math.{log10, ceil, floor, pow, abs, signum}
    val n = num.toDouble(x)
    
    // Determine the next lowest power of ten
    val order = pow(10, floor(log10(abs(n))))*signum(n)
    
    // Order is same sign as n so n/order is positive
    (ceil(n/order) toInt match {
      case 0 => 1    // Really shouldn't get here
      case 1 => 1
      case 2 => 2
      case d if 3 to 5 contains d => 5
      case _ => 10
    }) * order
  }
  
  /** Generate a histogram (ie. a frequency count) from a set of values, with the specified minimum number of bins */ 
  /* def apply[T](xs: Iterable[T], minBins: Int = 8)( implicit num: Numeric[T] ): Histogram = {
    val (lb, ub) = (xs.min, xs.max)
    val (lx, ux) = (num.toDouble(lb), num.toDouble(ub))
    val interval = ux-lx
    val binSize = 1.0/f10ceil(minBins/interval)
    
    val histMap = xs map { x => (num.toDouble(x)/binSize) toInt } groupBy(identity)
    val histRange: Range = {xs: Set[Int] => xs.min to xs.max} apply histMap.keySet
    Histogram(histRange map { i: Int => (i*binSize, histMap.getOrElse(i, Nil).size + 0.0)})
  } */
  /** Generate a histogram from a set of weighted values, ie. summing the weights */
  def apply[T](xs: Iterable[T], inws: Iterable[T] = Nil, minBins: Int = 8)( implicit num: Numeric[T]): Histogram = {
    val (lb, ub) = (xs.min, xs.max)
    val (lx, ux) = (num.toDouble(lb), num.toDouble(ub))
    val interval = ux-lx
    val binSize = 1.0/f10ceil(minBins/interval)
    
    val ws: Iterable[T] = inws match {
      case Nil => xs map (z => num.fromInt(1))
      case _   => inws
    }
    
    val histMap = xs zip ws map { case (x, w) => ((num.toDouble(x)/binSize) toInt, w) } groupBy { case (x, w) => x }
    val histRange: Range = {xs: Set[Int] => xs.min to xs.max} apply histMap.keySet
    // Histogram(histRange map { i: Int => (i*binSize, num.toDouble( (histMap.getOrElse(i, List((0,0.0))) map { case (i: Int, w: T) => w }).sum )) } )
    Histogram(histRange.head, binSize, histRange map { 
        i: Int => num.toDouble( (histMap.getOrElse(i, List((0,0.0))) map { case (i: Int, w: T) => w }).sum )
      } )
  }
}

// case class Histogram(data: Iterable[(Double, Double)]) {
case class Histogram(firstBin: Double, binSize: Double, data: Iterable[Double]) {
  
  override def toString = {
    
    def formatBar: ((Double, Double)) => String = { 
      case (v, n) => { 
        // ("%."+places+"f +/- %."+places+"f").format(rint(v*order)/order, rint(u*order)/order)
        // vtmp.substring(0, math.min(4, vtmp.length)) + ":" + (1 to n).map(_ => "o").mkString
        ("%.2f:").format(v) + (1 to n.toInt).map(_ => "o").mkString
      }
    }
    val bins = for (i <- 0 until data.size) yield firstBin + i*binSize
      
    bins.zip(data).toList sortBy (_._1) map (formatBar(_)) mkString "\n"
  }
  
  def numBins = data.size
}