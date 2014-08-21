import org.scalatest._
import org.scalatest.prop.GeneratorDrivenPropertyChecks

class MinEditPropertyTests extends FunSuite with ShouldMatchers with GeneratorDrivenPropertyChecks {

  // Ensure different classes sum up to the correct edit count.
  forAll { (commonClasses: Set[String], sourceClasses: Set[String], targetClasses: Set[String]) =>

    whenever (
      !commonClasses.contains("") &&
      !sourceClasses.contains("") &&
      !targetClasses.contains("") &&
      (commonClasses & sourceClasses & targetClasses) == Set.empty
    ) {

      val sourceElement = Element("tagName", classes = commonClasses ++ sourceClasses)
      val targetElement = Element("tagName", classes = commonClasses ++ targetClasses)

      sourceElement.getSwapCostTo(targetElement) should equal (sourceClasses.size + targetClasses.size)
    }

  }

  // Ensure arbitrary tags and ids compute the proper cost
  forAll { (tagName: String, id: Option[String], targetTagName: String, targetId: Option[String]) =>

    whenever (!tagName.isEmpty && !targetTagName.isEmpty && id != Some("") && targetId != Some("")) {

      val sourceElement = Element(tagName, id)
      val targetElement = Element(targetTagName, targetId)

      val targetCost = if (tagName != targetTagName) 1 else 0

      val idCost = if (id.isDefined && targetId.isDefined) {
          if (id != targetId) 2 else 0
        } else if (id.isDefined || targetId.isDefined) {
          1
        } else {
          0
        }

      sourceElement.getSwapCostTo(targetElement) should equal (targetCost + idCost)
    }

  }
}
