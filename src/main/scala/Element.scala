import java.util.regex.Pattern

object Element {
  val pattern = Pattern.compile("""(?<tagName>\w+)(?<id>\#\w+)?(?<classes>.*)""")

  def parseElement(elementStr: String): Element = {
    // TODO: ensure this allows correct chars

    val matcher = pattern.matcher(elementStr)

    if (matcher.matches()) {
      // Hacky - should be able to use the regex to better effect
      val tagName = matcher.group("tagName")

      val id = Option(matcher.group("id"))
        .map(_.replace("#", ""))

      val classes = Option(matcher.group("classes"))
        .map(_.split("\\.").toList)
        .getOrElse(Nil)
        .filter(_.length > 0)

      Element(
        tagName,
        id,
        classes)
    } else {
      throw new ElementException("Invalid element " + elementStr)
    }
  }
}

case class Element(tagName: String, id: Option[String] = None, classes: List[String] = Nil)
