package io

package object gearpump {
  type TimeStamp = Long
  val LatestTime = -1

  case class WorkerId(id: Int, registerTime: Long)

  object WorkerId {
    def unspecified: WorkerId = new WorkerId(-1, 0L)

    implicit val workerIdOrdering: Ordering[WorkerId] = {
      new Ordering[WorkerId] {

        /** Compare timestamp first, then id */
        override def compare(x: WorkerId, y: WorkerId): Int = {
          if (x.registerTime < y.registerTime) {
            -1
          } else if (x.registerTime == y.registerTime) {
            if (x.id < y.id) {
              -1
            } else if (x.id == y.id) {
              0
            } else {
              1
            }
          } else {
            1
          }
        }
      }
    }
  }
}
