
import Casino.GuestId

import scala.annotation.tailrec

case class Restaurant(nbSeats: Int) {
  def computeDayGains(payingGuests: Array[Int], guestMovements: Array[GuestId]): Int = {
    compute(payingGuests, guestMovements, State(0)).totalPayed
  }

  @tailrec final def compute(payingGuests: Array[Int], guestMovements: Array[GuestId], state: State = State(0)): State = {
    guestMovements match {
      // a guest that was queuing starts eating
      case Array(guestId, _*) if !state.isQueueEmpty && state.isSeatAvailable(nbSeats) => {
        val oldestQueuingGuest: GuestId = state.queuingGuests.head

        val newState: State =
          state
            .guestEats(oldestQueuingGuest)
            .guestLeavesTheQueue(oldestQueuingGuest)

        compute(payingGuests, guestMovements, newState)
      }

      // a guest leaves the queue
      case Array(guestId, _*) if state.isQueuing(guestId) =>
        val newState: State =
          state
            .guestLeavesTheQueue(guestId)

        compute(payingGuests, nextGuest(guestMovements), newState)

      // a guest has finished eating
      case Array(guestId, _*) if state.isEating(guestId) =>
        val newState: State =
          state
            .guestStopsEating(guestId)
            .guestPays(guestId, payingGuests)
            .guestHaveBeenEating(guestId)

        compute(payingGuests, nextGuest(guestMovements), newState)

      // a guest starts eating
      case Array(guestId, _*) if state.isSeatAvailable(nbSeats) =>
        val newState: State =
          state
            .guestEats(guestId)

        compute(payingGuests, guestMovements.drop(1), newState)

      // a guest starts queuing
      case Array(guestId, _*) if !state.isSeatAvailable(nbSeats) => {
        val newState: State =
          state
            .guestStartsQueuing(guestId)

        compute(payingGuests, nextGuest(guestMovements), newState)
      }

      // everybody has been served and is happy
      case _ =>
        state
    }
  }

  def nextGuest(guestMovements: Array[GuestId]): Array[GuestId] = guestMovements.drop(1)
}
