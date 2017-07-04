package countvotes.structures

case class Candidate(val name: String, val id: Option[Int] = None, val party: Option[String] = None) {
  id match {
    case Some(i) => {
      party match {
        case Some(p) => {
          def toString: String = "[" + i + "," + name + "," + p + "]"
        }
        case None => {
          def toString: String = "[" + i + "," + name + "]"
        }
      }
    }
    case None => {
      party match {
        case Some(p) => {
          def toString: String = "[" + name + "," + p + "]"
        }
        case None => {
          def toString: String = name
        }
      }
    }
  }
}

//object Candidate {
//  def apply(name: String, id: Option[Int] = None,  party: Option[String] = None) = new Candidate(name,id,party)
//}

