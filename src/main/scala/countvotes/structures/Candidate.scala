package countvotes.structures

case class Candidate(val name: String, val id: Option[Int] = None, val party: Option[String] = None) {
  override def toString: String = Seq(id, Some(name), party) filter{_.isDefined} map{_.get} mkString("[",",","]")
  /***
  override def toString:String = id match {
    case Some(i) => {
      party match {
        case Some(p) => "[" + i + "," + name + "," + p + "]"
        case None => "[" + i + "," + name + "]"
      }
    }
    case None => {
      party match {
        case Some(p) => "[" + name + "," + p + "]"
        case None => name
      }
    }
    }
    ***/
}

//object Candidate {
//  def apply(name: String, id: Option[Int] = None,  party: Option[String] = None) = new Candidate(name,id,party)
//}

