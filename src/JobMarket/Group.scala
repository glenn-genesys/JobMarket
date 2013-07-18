package JobMarket

import JobSim.normRand

case class Group( name: String, size: Int, numDisciplines: Int, homogeneity: Double ) {

  val archetype = (1 to numDisciplines) map ( _ => 1.0/normRand(1.0, 0.2) abs )
  
  val members = (1 to size) map (i => Worker(name + i.toString, (1 to numDisciplines) map ( _ => 1.0/normRand(1.0, 0.2) abs ) toList )) toList
  
}