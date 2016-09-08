package countvotes.structures

case class Candidate(val name: String, val id: Option[Int] = None, val party: Option[String] = None) {
  override def toString = name
  
  def canEqual(a: Any) = a.isInstanceOf[Candidate]
    override def equals(that: Any): Boolean =
        that match {
            case that: Candidate => that.canEqual(this) && this.name == that.name
            case _ => false
     }

}

//object Candidate {
//  def apply(name: String, id: Option[Int] = None,  party: Option[String] = None) = new Candidate(name,id,party)
//}