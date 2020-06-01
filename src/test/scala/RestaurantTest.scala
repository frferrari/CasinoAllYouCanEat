import org.scalatest.{Assertion, Matchers, WordSpec}

class RestaurantTest extends WordSpec with Matchers {
  val restaurant: Restaurant = new Restaurant(2)

  "A restaurant with 2 seats available" when {
    "given 2 guests for lunch" should {
      "serve this 2 guests" in {
        val state: State = restaurant.compute(Array(1, 2), Array(1, 2, 2, 1))

        state.totalPayed shouldEqual 3
        state.eatingGuests.isEmpty shouldEqual true
        state.haveBeenEatingGuests should contain theSameElementsAs List(1, 2)
        state.queuingGuests.isEmpty shouldEqual true
      }
    }

    "given 2 guests for lunch and 1 guest queuing/leaving without waiting for a free seat" should {
      "serve this 2 guests" in {
        val state: State = restaurant.compute(Array(1, 2, 4), Array(1, 2, 3, 3, 2, 1))

        state.totalPayed shouldEqual 3
        state.eatingGuests.isEmpty shouldEqual true
        state.haveBeenEatingGuests should contain theSameElementsAs List(1, 2)
        state.queuingGuests.isEmpty shouldEqual true
      }
    }
  }
}
