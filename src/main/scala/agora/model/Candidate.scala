package agora.model

case class Candidate(val name: String, val id: Option[Int] = None, val party: Option[String] = None) {
  override def toString: String = Seq(id, Some(name), party) filter{_.isDefined} map{_.get} mkString("[",",","]")
}
