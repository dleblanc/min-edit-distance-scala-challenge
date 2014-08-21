import java.util.regex.Pattern

class ElementException(msg: String) extends RuntimeException(msg)

object Element {
  val elementPattern =
    """(?<tagName>[\w-]+)""" +
    """((?:\#)(?<id>[\w-]+))?""" +
    """(?<classes>((?:\.)([\w-]+))*)"""

  val elementRegex = Pattern.compile(elementPattern)

  def parseElement(elementStr: String): Element = {

    val matcher = elementRegex.matcher(elementStr)

    if (matcher.matches()) {

      val tagName = matcher.group("tagName")

      val id = Option(matcher.group("id"))

      val classes = Option(matcher.group("classes"))
        .map(_.split("\\.").toSet)
        .getOrElse(Nil)
        .filter(_.length > 0)
        .toSet

      Element(tagName, id, classes)

    } else {
      throw new ElementException("Invalid element " + elementStr)
    }
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
