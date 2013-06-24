package JobMarket

object Estimate {
  def byWeight(v: Double, w: Double) = Estimate(v, math.sqrt(1.0/w))
}

case class Estimate(v: Double, s: Double) {
  def w = 1.0/math.pow(s,2)
  
  override def toString = v.toString + " +/- " + s
}