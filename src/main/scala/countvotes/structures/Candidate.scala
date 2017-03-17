package countvotes.structures

case class Candidate(val name: String, val id: Option[Int] = None, val party: Option[String] = None) {
  override def toString: String = name 
}

//object Candidate {
//  def apply(name: String, id: Option[Int] = None,  party: Option[String] = None) = new Candidate(name,id,party)
//}

