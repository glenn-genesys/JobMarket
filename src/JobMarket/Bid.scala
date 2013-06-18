package JobMarket

case class Bid( worker: Worker, job: Job, price: Double = 0.0 ) {
  def timeload = job.workerTime(worker)
  def workload = job.workload
  def creditRate = price/timeload
  def productionRate = workload/price
  def payoffRatio = workload*timeload/math.pow(price, 2)
}