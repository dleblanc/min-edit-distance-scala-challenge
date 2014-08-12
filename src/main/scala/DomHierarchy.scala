import java.util.regex.{Matcher, Pattern}

import scala.annotation.tailrec

class ElementException(msg: String) extends RuntimeException(msg)

object DomHierarchy {

  def fromString(domStr: String): DomHierarchy = {
    DomHierarchy(domStr
      .split(" ")
      .map(Element.parseElement)
      .toList)
  }

  def getHierarchyEditDistance(h1: DomHierarchy, h2: DomHierarchy): Int = {

    val elementEditCount = getElementEditCount(h1.elements.map(_.tagName), h2.elements.map(_.tagName))

    elementEditCount
  }

  // Using a naieve Levenshtien Distance computation. Not very computationally friendly. DP is another option.
  // Also note: would prefer to use tail recursion here
  //@tailrec
  def getElementEditCount(tagsH1: Seq[String], tagsH2: Seq[String]): Int = {
    if (tagsH1.length <= 0) {
      return tagsH2.length
    }
    if (tagsH2.length <= 0) {
      return tagsH1.length
    }

    val headChangeCount = if (tagsH1.head == tagsH2.head) 1 else 0

    minMany(
      getElementEditCount(tagsH1.tail, tagsH2.tail) + headChangeCount,
      getElementEditCount(tagsH1.tail, tagsH2) + 1,
      getElementEditCount(tagsH1, tagsH2.tail) + 1
    )
  }

  def minMany(items: Int*): Int = {
    items.foldLeft(Int.MaxValue) { case (a, b) =>
        math.min(a, b)
    }
  }
}

case class DomHierarchy(elements: List[Element])


