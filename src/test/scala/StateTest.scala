import Casino.GuestId
import org.scalatest.{Matchers, WordSpec}

class StateTest extends WordSpec with Matchers {
  /**
   * EATING guests
   */

  "A State with 0 guests eating" when {
    val state: State = State(0)

    "checked if guest 100 is eating" should {
      "return false" in {
        state.isEating(100) shouldEqual false
      }
    }
  }

  "A State with 2 guests eating (ids=100 and 101)" when {
    val state: State = State(0, List(100, 101))

    "checked if guest 100 is eating" should {
      "return true " in {
        state.isEating(100) shouldEqual true
      }
    }
    "checked if guest 101 is eating" should {
      "return true " in {
        state.isEating(101) shouldEqual true
      }
    }
    "checked if guest 102 is eating" should {
      "return false" in {
        state.isEating(102) shouldEqual false
      }
    }
  }

  /**
   * COMING BACK guests
   */

  "A State with 0 guests having been eating" when {
    val state: State = State(0)

    "checked if guest 100 is coming back" should {
      "return false" in {
        state.isComingBack(100) shouldEqual false
      }
    }
  }

  "A State with 2 guests having been eating (ids=100 and 101)" when {
    val state: State = State(0, haveBeenEatingGuests = List(100, 101))

    "checked if guest 100 is coming back" should {
      "return true" in {
        state.isComingBack(100) shouldEqual true
      }
    }

    "checked if guest 101 is coming back" should {
      "return true" in {
        state.isComingBack(101) shouldEqual true
      }
    }

    "checked if guest 102 is coming back" should {
      "return false" in {
        state.isComingBack(102) shouldEqual false
      }
    }
  }

  /**
   * QUEUING guest
   */

  "A State with 0 guests queuing" when {
    val state: State = State(0)

    "checked if guest 100 is queuing" should {
      "return false" in {
        state.isQueuing(100) shouldEqual false
      }
    }

    "checked if the queue is empty" should {
      "return true" in {
        state.isQueueEmpty shouldEqual true
      }
    }
  }

  "A State with 2 guests queuing (ids=100 and 101)" when {
    val state: State = State(0, queuingGuests = List(100, 101))

    "checked if guest 100 is queuing" should {
      "return true" in {
        state.isQueuing(100) shouldEqual true
      }
    }

    "checked if guest 101 is queuing" should {
      "return true" in {
        state.isQueuing(101) shouldEqual true
      }
    }

    "checked if guest 102 is queuing" should {
      "return false" in {
        state.isQueuing(102) shouldEqual false
      }
    }

    "checked if the queue is empty" should {
      "return false" in {
        state.isQueueEmpty shouldEqual false
      }
    }
  }

  /**
   * GUEST EATS
   */

  "A State with no guests eating" when {
    val queuingGuests: List[GuestId] = List(105)
    val haveBeenEatingGuests: List[GuestId] = List(106)
    val state: State = State(0, queuingGuests = queuingGuests, haveBeenEatingGuests = haveBeenEatingGuests)

    "asked to make guest 100 eat" should {
      "add guest 100 to its collection of eating guests" in {
        state
          .guestEats(100) shouldBe State(0, eatingGuests = List(100), queuingGuests = queuingGuests, haveBeenEatingGuests = haveBeenEatingGuests)
      }
    }

    "asked to make guest 100 eat then guest 101 eat" should {
      "add guest 100 and 101 to its collection of eating guests" in {
        state
          .guestEats(100)
          .guestEats(101) shouldBe State(0, eatingGuests = List(100, 101), queuingGuests = queuingGuests, haveBeenEatingGuests = haveBeenEatingGuests)
      }
    }
  }

  /**
   * GUEST PAYS
   */

  "A State with an empty list of guest having been eating" when {
    val queuingGuests: List[GuestId] = List(105)
    val eatingGuests: List[GuestId] = List(106)
    val state: State = State(0, eatingGuests = eatingGuests, queuingGuests = queuingGuests)

    "asked to make guest 2 pay" should {
      "make this guest pay and update the state totalPayed amount" in {
        state.guestPays(2, payingGuests = Array(5, 25)) shouldBe State(25, eatingGuests = eatingGuests, queuingGuests = queuingGuests)
      }
    }
    "asked to make guest 2 pay then guest 1 pay" should {
      "make this 2 guests pay and update the state totalPayed amount" in {
        state
          .guestPays(2, payingGuests = Array(5, 25))
          .guestPays(1, payingGuests = Array(5, 25)) shouldBe State(30, eatingGuests = eatingGuests, queuingGuests = queuingGuests)
      }
    }
  }

  "A State with guest 2 as a coming back guest" when {
    val queuingGuests: List[GuestId] = List(105)
    val eatingGuests: List[GuestId] = List(106)
    val state: State = State(10, haveBeenEatingGuests = List(2), eatingGuests = eatingGuests, queuingGuests = queuingGuests)

    "asked to make guest 2 pay" should {
      "not make this guest pay and leave the state original totalPayed unchanged" in {
        state.guestPays(2, payingGuests = Array(5, 25)) shouldBe State(10, haveBeenEatingGuests = List(2), eatingGuests = eatingGuests, queuingGuests = queuingGuests)
      }
    }
    "asked to make guest 2 pay then guest 1 pay" should {
      "not make guest 2 pay and make guest 1 pay and update the state totalPayed amount" in {
        state
          .guestPays(2, payingGuests = Array(5, 25))
          .guestPays(1, payingGuests = Array(5, 25)) shouldBe State(15, haveBeenEatingGuests = List(2), eatingGuests = eatingGuests, queuingGuests = queuingGuests)
      }
    }
  }

  /**
   * GUEST STOPS EATING
   */

  "A State with 2 guests eating (ids=100, 101)" when {
    val queuingGuests: List[GuestId] = List(105)
    val haveBeenEatingGuests: List[GuestId] = List(106)
    val state: State = State(10, eatingGuests = List(100, 101), queuingGuests = queuingGuests, haveBeenEatingGuests = haveBeenEatingGuests)

    "asked to make guest 100 stop eating" should {
      "return a new state having only guest 101 eating" in {
        state.guestStopsEating(100) shouldBe State(10, eatingGuests = List(101), queuingGuests = queuingGuests, haveBeenEatingGuests = haveBeenEatingGuests)
      }
    }

    "asked to make guests 100 and 101 stop eating" should {
      "return a new state having no guest eating" in {
        state
          .guestStopsEating(100)
          .guestStopsEating(101) shouldBe State(10, eatingGuests = List.empty[GuestId], queuingGuests = queuingGuests, haveBeenEatingGuests = haveBeenEatingGuests)
      }
    }

    "asked to make guests 100 and 101 and 102 stop eating" should {
      "return a new state having no guest eating" in {
        state
          .guestStopsEating(100)
          .guestStopsEating(101)
          .guestStopsEating(102) shouldBe State(10, eatingGuests = List.empty[GuestId], queuingGuests = queuingGuests, haveBeenEatingGuests = haveBeenEatingGuests)
      }
    }
  }

  /**
   * GUEST HAVE BEEN EATING
   */

  "A State with guest 106 who have already been eating" when {
    val eatingGuests: List[GuestId] = List(100, 101)
    val queuingGuests: List[GuestId] = List(105)
    val haveBeenEatingGuests: List[GuestId] = List(106)
    val state: State = State(10, eatingGuests = eatingGuests, queuingGuests = queuingGuests, haveBeenEatingGuests = haveBeenEatingGuests)

    "asked to update for guest 106 having eating" should {
      "return an unchanged state" in {
        state.guestHaveBeenEating(106) shouldBe state
      }
    }

    "asked to update for guest 100 having eating" should {
      "return an unchanged state" in {
        state.guestHaveBeenEating(100) shouldBe State(10, eatingGuests = eatingGuests, queuingGuests = queuingGuests, haveBeenEatingGuests = List(106, 100))
      }
    }
  }

  /**
   * GUEST STARTS QUEUING
   */

  "A State with no guests queuing" when {
    val eatingGuests: List[GuestId] = List(100, 101)
    val queuingGuests: List[GuestId] = List(105)
    val haveBeenEatingGuests: List[GuestId] = List(106)
    val state: State = State(10, eatingGuests = eatingGuests, haveBeenEatingGuests = haveBeenEatingGuests)

    "asked to make guest 105 queue" should {
      "return a new state with only guest 105 queuing" in {
        state.guestStartsQueuing(105) shouldBe State(10, eatingGuests = eatingGuests, queuingGuests = queuingGuests, haveBeenEatingGuests = haveBeenEatingGuests)
      }
    }
  }

  "A State with guest 105 already queuing" when {
    val eatingGuests: List[GuestId] = List(100, 101)
    val queuingGuests: List[GuestId] = List(105)
    val haveBeenEatingGuests: List[GuestId] = List(106)
    val state: State = State(10, eatingGuests = eatingGuests, queuingGuests = queuingGuests, haveBeenEatingGuests = haveBeenEatingGuests)

    "asked to make guest 108 queue" should {
      "return a new state with guest 108 added to the queuing guests" in {
        state.guestStartsQueuing(108) shouldBe State(10, eatingGuests = eatingGuests, queuingGuests = List(105, 108), haveBeenEatingGuests = haveBeenEatingGuests)
      }
    }
  }

  /**
   * GUEST LEAVES THE QUEUE
   */

  "A State with 2 guests queuing (ids=100, 101)" when {
    val eatingGuests: List[GuestId] = List(104, 105)
    val queuingGuests: List[GuestId] = List(100, 101)
    val haveBeenEatingGuests: List[GuestId] = List(106)
    val state: State = State(10, eatingGuests = eatingGuests, queuingGuests = queuingGuests, haveBeenEatingGuests = haveBeenEatingGuests)

    "asked to make guest 100 leave the queue" should {
      "return a new state having only guest 101 queuing" in {
        state.guestLeavesTheQueue(100) shouldBe State(10, eatingGuests = eatingGuests, queuingGuests = List(101), haveBeenEatingGuests = haveBeenEatingGuests)
      }
    }

    "asked to make guests 100 and 101 leave the queue" should {
      "return a new state having no guest queuing" in {
        state
          .guestLeavesTheQueue(100)
          .guestLeavesTheQueue(101) shouldBe State(10, eatingGuests = eatingGuests, queuingGuests = List.empty[GuestId], haveBeenEatingGuests = haveBeenEatingGuests)
      }
    }

    "asked to make guests 100 and 101 and 102 stop queuing" should {
      "return a new state having no guest eating" in {
        state
          .guestLeavesTheQueue(100)
          .guestLeavesTheQueue(101)
          .guestLeavesTheQueue(102) shouldBe State(10, eatingGuests = eatingGuests, queuingGuests = List.empty[GuestId], haveBeenEatingGuests = haveBeenEatingGuests)
      }
    }
  }
}