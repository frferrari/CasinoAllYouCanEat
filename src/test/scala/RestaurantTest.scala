import Casino.GuestId
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
        val state: State = restaurant.compute(Array(10, 15, 20), Array(1, 2, 3, 3, 2, 1), State(0))

        state.totalPayed shouldEqual 25
        state.eatingGuests.isEmpty shouldEqual true
        state.haveBeenEatingGuests should contain theSameElementsAs List(1, 2)
        state.queuingGuests.isEmpty shouldEqual true
      }
    }

    "given 2 guests for lunch and 1 guest queuing/leaving without eating then coming back and eating" should {
      "serve this 2 guests" in {
        val state: State = restaurant.compute(Array(10, 15, 20), Array(1, 2, 3, 3, 2, 3, 1, 3))

        state.totalPayed shouldEqual 45
        state.eatingGuests.isEmpty shouldEqual true
        state.haveBeenEatingGuests should contain theSameElementsAs List(1, 2, 3)
        state.queuingGuests.isEmpty shouldEqual true
      }
    }

    "given 2 guests for lunch and 1 guest queuing/leaving without eating then coming back immediately and eating" should {
      "serve this 2 guests" in {
        val state: State = restaurant.compute(Array(10, 15, 20), Array(1, 2, 3, 3, 3, 2, 3, 1))

        state.totalPayed shouldEqual 45
        state.eatingGuests.isEmpty shouldEqual true
        state.haveBeenEatingGuests should contain theSameElementsAs List(1, 2, 3)
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

    "A restaurant with 2 seats available" when {
      "given an empty list of guest" should {
        "return an empty list of guest" in {
          restaurant.nextGuest(Array.empty[GuestId]) shouldEqual (Array.empty[GuestId])
        }
      }

      "given 1 guest" should {
        "return an empty array of guest" in {
          restaurant.nextGuest(Array(100)) shouldEqual (Array.empty[GuestId])
        }
      }

      "given 2 guests" should {
        "return aa array of 1 guest, dropping the first guest from the original array" in {
          restaurant.nextGuest(Array(100, 101)) shouldEqual (Array(101))
        }
      }

      "given 8 guests" should {
        "return an array of 7 guests, dropping the first guest from the original array" in {
          restaurant.nextGuest(Array(100, 101, 102, 103, 104, 105, 106, 107)) shouldEqual (Array(101, 102, 103, 104, 105, 106, 107))
        }
      }
    }

    "A restaurant with 2 seats" when {
      "having an empty list of eating quest and queried for an available seat" should {
        "return true" in {
          val restaurant: Restaurant = new Restaurant(2)
          restaurant.isSeatAvailable(restaurant.restaurantState) shouldEqual true
        }
      }

      "having 1 guest eating and queried for an available seat" should {
        "return true" in {
          val restaurant: Restaurant = new Restaurant(2, State(0, List(100)))
          restaurant.isSeatAvailable(restaurant.restaurantState) shouldEqual true
        }
      }

      "having 2 guests eating and queried for an available seat" should {
        "return false" in {
          val restaurant: Restaurant = new Restaurant(2, State(0, List(100, 101)))
          restaurant.isSeatAvailable(restaurant.restaurantState) shouldEqual false
        }
      }
    }
  }
}
