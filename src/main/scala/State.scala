import Casino.GuestId

case class State(totalPayed: Int,
                 eatingGuests: List[GuestId] = List.empty[GuestId],
                 queuingGuests: List[GuestId] = List.empty[GuestId],
                 haveBeenEatingGuests: List[GuestId] = List.empty[GuestId]) {

  def isEating(guestId: GuestId): Boolean = eatingGuests.contains(guestId)

  def isComingBack(guestId: GuestId): Boolean = haveBeenEatingGuests.contains(guestId)

  def isQueuing(guestId: GuestId): Boolean = queuingGuests.contains(guestId)

  def isQueueEmpty: Boolean = queuingGuests.isEmpty

  def guestEats(guestId: GuestId): State =
    this.copy(eatingGuests = this.eatingGuests :+ guestId)

  def guestPays(guestId: GuestId, payingGuests: Array[Int]): State = {
    if (isComingBack(guestId))
      this
    else
      this.copy(
        totalPayed = this.totalPayed + payingGuests(guestId - 1)
      )
  }

  def guestStopsEating(guestId: GuestId): State =
    this.copy(eatingGuests = this.eatingGuests.filter(_ != guestId))

  def guestHaveBeenEating(guestId: Int): State =
    if (isComingBack(guestId))
      this
    else
      this.copy(haveBeenEatingGuests = this.haveBeenEatingGuests :+ guestId)

  def guestStartsQueuing(guestId: GuestId): State =
    this.copy(
      queuingGuests = this.queuingGuests :+ guestId
    )

  def guestLeavesTheQueue(guestId: GuestId): State =
    this.copy(
      queuingGuests = this.queuingGuests.filter(_ != guestId)
    )
}
