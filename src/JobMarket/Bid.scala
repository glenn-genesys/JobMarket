package JobMarket

case class Bid( worker: Worker, job: Job, price: Double = 0.0 ) {
  val timeload = job.workerTime(worker)
  val workload = job.workload
  val creditRate = price/timeload
  val productionRate = workload/price
  val value = workload/timeload
  val payoffRatio = workload*timeload/math.pow(price, 2)
}