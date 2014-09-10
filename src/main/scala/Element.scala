object Element {

  def parseElement(elementStr: String): Element = {

    val tagAndId :: classes = elementStr.split("""\.""").toList
    val tagName :: idList = tagAndId.split("""#""").toList

    Element(tagName, idList.headOption, classes.toSet)
  }
}

case class Element(tagName: String, id: Option[String] = None, classes: Set[String] = Set.empty) {

  def getDelCost = 1

  def getAddCost = 1 + id.map(_ => 1).getOrElse(0) + classes.size

  def getSwapCostTo(that: Element): Int = {

    val tagCost = if (this.tagName != that.tagName) 1 else 0

    val idCost = (this.id, that.id) match {
      case (Some(id1), Some(id2)) if id1 != id2 => 2
      case (Some(_), None) | (None, Some(_))  => 1
      case _ => 0
    }
    val classesCost = this.classes.diff(that.classes).size + that.classes.diff(this.classes).size

    tagCost + idCost + classesCost
  }

}
