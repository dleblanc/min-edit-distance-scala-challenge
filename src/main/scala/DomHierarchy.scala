import java.util.regex.{Matcher, Pattern}

class ElementException(msg: String) extends RuntimeException(msg)

object DomHierarchy {

  val elementPattern = Pattern.compile("""(\w+)""")

//  def fromString(domStr: String): DomHierarchy = {
//    domStr
//      .split(" ")
//      .map(parseElement(_))
//  }
  def fromString(domStr: String): DomHierarchy = ???

  def parseElement(elementStr: String): Element = {
    // TODO: ensure this allows correct chars
    //val elementPattern = Pattern.compile("""(\w+)(#\w+)?(\.\w+)*""")

    val pattern = Pattern.compile("""(?<tagName>\w+)(?<id>\#\w+)?(?<classes>.*)""")

    val matcher = pattern.matcher(elementStr)

    if (matcher.matches()) {
      // Hacky - should be able to strip out # with non capturing group
      val tagName = matcher.group("tagName")

      val id = Option(matcher.group("id"))
        .map(_.replace("#", ""))

      val classes = Option(matcher.group("classes"))
        .map(_.split("\\.").toList)
        .getOrElse(Nil)
        .filter(_.length > 0)

      Element(
        matcher.group(1),
        id,
        classes)
    } else {
      throw new ElementException("Invalid element " + elementStr)
    }
  }

}

case class DomHierarchy(elements: List[Element])
case class Element(tagName: String, id: Option[String] = None, classes: List[String] = Nil)
