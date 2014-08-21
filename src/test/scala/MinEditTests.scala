import Element._
import org.scalatest._
import org.scalatest.prop.TableDrivenPropertyChecks._
import StrLev._

import scala.io.Source

class MinEditTests extends FunSuite with ShouldMatchers {

  val levenshteinTests = Table(
    ("s1", "s2", "distance"),
    ("", "", 0),
    ("a", "", 1),
    ("ab", "", 2),
    ("ab", "ab", 0),
    ("kitten", "sitting", 3),
    ("foo", "foof", 1),
    ("industry", "interest", 6)
  )

  forAll(levenshteinTests) { (first: String, second: String, distance: Int) =>

    test(s"Comparing strings '$first' to '$second' should yield an edit distance of $distance") {

      strLevDist(first, second) should equal(distance)
    }

  }

  val simpleHierarchyTests = Table(
    ("first", "second", "distance"),
    ("first", "first", 0),
    ("first", "different", 1),
    ("first second", "first", 1),
    ("first second", "first second", 0),
    ("first second third", "first third", 1),
    ("first third", "first second third", 1),
    ("firstTag#id", "firstTag", 1),
    ("firstTag#id", "firstTag#id", 0),
    ("div#id", "div", 1),
    ("div#id", "img.photo", 3),
    ("div#id img.photo", "a.btn", 4),
    ("firstTag#id secondTag", "firstTag#id secondTag", 0),
    ("firstTag#id secondTag", "firstTag secondTag", 1),
    ("div.footer.fixed a#signup.blue.btn", "div.header li.btn a#signup", 7),
    ("div.green.dotted a#login", "a#login div.green.dotted", 3)
  )

  forAll(simpleHierarchyTests) { (first: String, second: String, distance: Int) =>

    test(s"Comparing simple hierarchy '$first' to '$second' should yield an edit distance of $distance") {

      val h1 = DomHierarchy.fromString(first).elements
      val h2 = DomHierarchy.fromString(second).elements

      DomHierarchy.domHierarchyLevDist(h1, h2) should equal(distance)
    }
  }

  test("Run a specific simple hierarchy test") {

    val testNum = 14
    val (h1, h2, dist) = simpleHierarchyTests(testNum)
    val e1 = DomHierarchy.fromString(h1).elements
    val e2 = DomHierarchy.fromString(h2).elements

    withClue(s"using test# '$testNum, from: '$h1', to: '$h2'") {

      DomHierarchy.domHierarchyLevDist(e1, e2) should equal(dist)
    }

  }

  test("Compare swap costs for different tag names including an id from source and one class on target") {

    val e1 = Element("div", Some("#id"))
    val e2 = Element("img", classes = Set("photo"))

    e1.getSwapCostTo(e2) should equal(3)
  }

  test("Swap two different ids on same tag name yields 2") {

    val e1 = Element("div", Some("#id"))
    val e2 = Element("div", Some("#different"))

    e1.getSwapCostTo(e2) should equal(2)
  }

  test("Swap cost for same id is 0") {

    val e1 = Element("div", Some("#same"))
    val e2 = Element("div", Some("#same"))

    e1.getSwapCostTo(e2) should equal(0)
  }

  test("Swap cost for no ids is 0") {

    val e1 = Element("div")
    val e2 = Element("div")

    e1.getSwapCostTo(e2) should equal(0)
  }

  test("Swap cost for one id set the other none is 1") {

    Element("div", Some("#id")).getSwapCostTo(Element("div")) should equal(1)
    Element("div").getSwapCostTo(Element("div", Some("#id"))) should equal(1)
  }

  test("Parse element parses just the tag name") {
    parseElement("tagname") should equal(Element("tagname"))
  }

  test("Parse element parses tag and id") {
    parseElement("tagname#id") should equal(Element("tagname", id = Some("id")))
  }

  test("Parse element parses tag and class") {
    parseElement("tagname.class") should equal(Element("tagname", classes = Set("class")))
  }

  test("Parse element parses tag and multiple classes") {
    parseElement("tagname.class1.class2") should equal(Element("tagname", classes = Set("class1", "class2")))
  }

  test("Parse element parses tag, id and a class") {
    parseElement("tagname#id.class") should equal(Element("tagname", id = Some("id"), classes = Set("class")))
  }

  test("Parse element parses tag, id and multiple classes") {
    parseElement("tagname#id.class1.class2") should equal(Element("tagname", id = Some("id"), classes = Set("class1", "class2")))
  }

  test("Parses a dom hierarchy from a list of element descriptors") {
    val elements = DomHierarchy.fromString("firstTag#firstId.firstClass1.firstClass2 secondTag#id.secondClass1.secondClass2").elements
    elements.head should equal(Element("firstTag", id = Some("firstId"), classes = Set("firstClass1", "firstClass2")))
  }


  val hierarchiesAndEditDistance = Source
    .fromInputStream(getClass.getResourceAsStream("/edits.txt"))
    .getLines()
    .grouped(3)
    .zipWithIndex
    .map { case (group, testNum) =>
    (testNum, group(0), group(1), group(2).toInt)
  }
    .toSeq

  val domHierarchiesAndEdits =
    Table(
      ("test num", "h1", "h2", "distance"), // First tuple defines column names
      hierarchiesAndEditDistance: _*
    )

  forAll(domHierarchiesAndEdits) { (testNum: Int, h1Str: String, h2Str: String, distance: Int) =>

    test(s"[$testNum] Comparing '$h1Str' to '$h2Str' should yield an edit distance of $distance") {

      val h1 = DomHierarchy.fromString(h1Str)
      val h2 = DomHierarchy.fromString(h2Str)

      DomHierarchy.domHierarchyLevDist(h1.elements, h2.elements) should equal(distance)
    }

  }

  test("Run a specific complex hierarchy test") {

    val testToRun = 10
    val (testNum, h1, h2, dist) = hierarchiesAndEditDistance(testToRun)
    val e1 = DomHierarchy.fromString(h1).elements
    val e2 = DomHierarchy.fromString(h2).elements

    withClue(s"using test# '$testNum, from: '$h1', to: '$h2'") {

      DomHierarchy.domHierarchyLevDist(e1, e2) should equal(dist)
    }

  }




}


