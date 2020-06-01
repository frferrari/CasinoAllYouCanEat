import org.scalatest.{Assertion, Matchers, WordSpec}

class RestaurantTest extends WordSpec with Matchers {
  val restaurant: Restaurant = new Restaurant(2)

  "A restaurant with 2 seats available" when {
    "given 2 guests for lunch" should {
      "serve this 2 guests" in {
        val state: State = restaurant.compute(Array(10, 15), Array(1, 2, 2, 1))

        state.totalPayed shouldEqual 25
        state.eatingGuests.isEmpty shouldEqual true
        state.haveBeenEatingGuests should contain theSameElementsAs List(1, 2)
        state.queuingGuests.isEmpty shouldEqual true
      }
    }

    "given 2 guests for lunch and 1 guest queuing/leaving without eating" should {
      "serve this 2 guests" in {
        val state: State = restaurant.compute(Array(10, 15, 20), Array(1, 2, 3, 3, 2, 1))

        state.totalPayed shouldEqual 25
        state.eatingGuests.isEmpty shouldEqual true
        state.haveBeenEatingGuests should contain theSameElementsAs List(1, 2)
        state.queuingGuests.isEmpty shouldEqual true
      }
    }

    "given 3 guests for lunch and 1 guest queuing/waiting for a free seat" should {
      "serve this 3 guests" in {
        val state: State = restaurant.compute(Array(10, 15, 20), Array(1, 2, 3, 2, 3, 1))

        state.totalPayed shouldEqual 45
        state.eatingGuests.isEmpty shouldEqual true
        state.haveBeenEatingGuests should contain theSameElementsAs List(1, 2, 3)
        state.queuingGuests.isEmpty shouldEqual true
      }
    }

    "given 4 guests for lunch and 2 guests queuing/waiting for a free seat" should {
      "serve this 4 guests" in {
        val state: State = restaurant.compute(Array(10, 15, 20, 15), Array(1, 2, 3, 4, 2, 3, 4, 1))

        state.totalPayed shouldEqual 60
        state.eatingGuests.isEmpty shouldEqual true
        state.haveBeenEatingGuests should contain theSameElementsAs List(1, 2, 3, 4)
        state.queuingGuests.isEmpty shouldEqual true
      }
    }

    "given 4 guests for lunch and 1 guest queuing/waiting for a free seat and 1 guest queuing/leaving without eating" should {
      "serve this 4 guests" in {
        val state: State = restaurant.compute(Array(10, 15, 20, 15), Array(1, 2, 3, 4, 4, 2, 3, 1))

        state.totalPayed shouldEqual 45
        state.eatingGuests.isEmpty shouldEqual true
        state.haveBeenEatingGuests should contain theSameElementsAs List(1, 2, 3)
        state.queuingGuests.isEmpty shouldEqual true
      }
    }
  }
}
