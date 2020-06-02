import org.scalatest.{Matchers, WordSpec}

class StateTest extends WordSpec with Matchers {
  /**
   * EATING guests
   */

  "A State with 0 guests eating" when {
    val state: State = new State(0)

    "checked if guest 100 is eating" should {
      "return false" in {
        state.isEating(100) shouldEqual false
      }
    }
  }

  "A State with 2 guests eating (ids=100 and 101)" when {
    val state: State = new State(0, List(100, 101))

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
    val state: State = new State(0)

    "checked if guest 100 is coming back" should {
      "return false" in {
        state.isComingBack(100) shouldEqual false
      }
    }
  }

  "A State with 2 guests having been eating (ids=100 and 101)" when {
    val state: State = new State(0, haveBeenEatingGuests = List(100, 101))

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
    val state: State = new State(0)

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
    val state: State = new State(0, queuingGuests = List(100, 101))

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
}
