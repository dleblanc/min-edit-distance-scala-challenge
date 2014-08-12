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


    // Get the element edit count
    val elementEditCount = getElementEditCount(h1.elements.map(_.tagName), h2.elements.map(_.tagName))

    // Then get any id counts
    val idEdits = getIdInsertOrRemovalCount(h1.elements.map(_.id), h2.elements.map(_.id))
    
    // Then get the classes count
    val classEdits = getClassInsertOrRemovalCount(h1.elements.map(_.classes), h2.elements.map(_.classes))

    elementEditCount + idEdits + classEdits
  }

  // Using a Levenshtien Distance computation - uses dynamic programming (naive one didn't work so well).
  def getElementEditCount(tagsH1: Seq[String], tagsH2: Seq[String]): Int = {

    val distances = Array.ofDim[Int](tagsH1.length + 1, tagsH2.length + 1)

    (0 to tagsH1.length)
      .foreach(i => distances(i)(0) = i)

    (0 to tagsH2.length)
      .foreach(j => distances(0)(j) = j)

    (1 to tagsH2.length) foreach { j =>
      (1 to tagsH1.length) foreach { i =>

        if (tagsH1(i-1) == tagsH2(j-1)) {
          distances(i)(j) = distances(i-1)(j-1)
        } else {
          distances(i)(j) = List(
            distances(i-1)(j) + 1,
            distances(i)(j-1) + 1,
            distances(i-1)(j-1) + 1
          ).min
        }

      }
    }

    distances(tagsH1.length)(tagsH2.length)
  }

  def getIdInsertOrRemovalCount(h1Ids: Seq[Option[String]], h2Ids: Seq[Option[String]]): Int = {

    val idPairs = h1Ids.zip(h2Ids)

    idPairs.map { case (id1, id2) =>
      if ((id1.isDefined && id2 == None) || (id1 == None && id2.isDefined)) {
        1
      } else if (id1 == None && id2 == None) {
        0
      } else {
        if (id1.get != id2.get) 1 else 0
      }
    }.sum
  }

  def getClassInsertOrRemovalCount(classesH1: List[List[String]], classesH2: List[List[String]]): Int = {
    val classesPairs = classesH1.zip(classesH2)

    classesPairs.map { case (classes1, classes2) =>

      // That ugly operator is the set difference operator. Uggh.
      val difference = Set(classes1: _*).diff(Set(classes2: _*))
      difference.size
    }.sum
  }

}

case class DomHierarchy(elements: List[Element])


