package org.aossie.agora.model

class Candidate(
    val name: String,
    val id: Option[Int] = None,
    val party: Option[String] = None
) {

  override def toString: String = Seq(id, Some(name), party)
    .filter(_.isDefined)
    .map {
      _.get
    }
    .mkString("[", ",", "]")

  def canEqual(a: Any) = a.isInstanceOf[Candidate]

  override def equals(that: Any): Boolean =
    that match {
      case that: Candidate =>
        that.canEqual(this) &&
        this.name == that.name &&
        this.id == that.id &&
        this.party == that.party
      case _ => false
    }

  override def hashCode(): Int = {
    val prime  = 31
    var result = 1
    result = prime * result + (if (name == null) 0 else name.hashCode)
    result = prime * result + (if (id.isEmpty) 0 else id.hashCode)
    result = prime * result + (if (party.isEmpty) 0 else party.hashCode)
    result
  }

}
