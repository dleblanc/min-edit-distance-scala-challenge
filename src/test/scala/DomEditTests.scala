import DomHierarchy._
import org.scalatest._
import org.scalatest.prop.TableDrivenPropertyChecks._

import scala.io.Source

class DomEditTests extends FunSuite with ShouldMatchers {

  val hierarchiesAndEditDistance = Source
    .fromInputStream(getClass.getResourceAsStream("/edits.txt"))
    .getLines()
    .grouped(3)
    .zipWithIndex
    .map { case (group, testNum) =>
    (testNum, group(0), group(1), group(2).toInt)
  }
    .toList

  val domHierarchiesAndEdits =
    Table(
      ("test num", "h1", "h2", "distance"), // First tuple defines column names
      hierarchiesAndEditDistance: _*
    )

  forAll(domHierarchiesAndEdits) { (testNum: Int, h1Str: String, h2Str: String, distance: Int) =>

    test(s"[$testNum] Comparing '$h1Str' to '$h2Str' should yield an edit distance of $distance") {


      //      val h1 = DomHierarchy.fromString(h1)
      //      val h2 = DomHierarchy.fromString(h2)
      //
      //      compareHierarchies(h1Str, h2Str) should equal (distance)
    }

  }


  test("Parse element parses just the tag name") {
    parseElement("tagname") should equal(Element("tagname"))
  }

  test("Parse element parses tag and id") {
    parseElement("tagname#id") should equal(Element("tagname", id = Some("id")))
  }

  test("Parse element parses tag and class") {
    parseElement("tagname.class") should equal(Element("tagname", classes = List("class")))
  }

  test("Parse element parses tag and multiple classes") {
    parseElement("tagname.class1.class2") should equal(Element("tagname", classes = List("class1", "class2")))
  }

  test("Parse element parses tag, id and a class") {
    parseElement("tagname#id.class") should equal(Element("tagname", id = Some("id"), classes = List("class")))
  }

  test("Parse element parses tag, id and multiple classes") {
    parseElement("tagname#id.class1.class2") should equal(Element("tagname", id = Some("id"), classes = List("class1", "class2")))
  }

}


