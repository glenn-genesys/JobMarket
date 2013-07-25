package JobMarket

import JobSim.{mean, std, wmeanstd}
import math.{pow, floor, max, rint, log10}

object Estimate {
  def byWeight(v: Double, w: Double) = Estimate(v, math.sqrt(1.0/w))
  
  // Alternate case constructor accepts Iterable[Numeric]
  def apply[T]( ds: Iterable[T])( implicit num: Numeric[T] ): Estimate = Estimate(mean(ds), std(ds))
  
  def apply( ds: Iterable[Estimate]): Estimate = wmeanstd(ds)
}

case class Estimate(v: Double, u: Double) {
  val eps = v*1e-5
  
  // Prevent undefined weights -- avoid division by zero uncertainty
  val w = 1.0/(math.pow(u,2) + eps)
  
  override def toString = {
      u match {
        case 0.0 => v.toString
        case _ => {
          // Format result with uncertainty  v +/- u
	      val sigFigures = 1-floor(log10(u))
	      val order: Double = pow(10, sigFigures)
	      val places = max(0,sigFigures).toInt.toString
	      ("%."+places+"f +/- %."+places+"f").format(rint(v*order)/order, rint(u*order)/order)
        }
      }
    }
}