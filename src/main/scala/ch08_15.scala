
import cats.effect.IO

object Ch08_15 {

  case class MeetingTime (start: Int, end: Int)


  def calendarEntriesApiCall(name: String): List[MeetingTime] = {
    println("start calendarEntriesApiCall")

    List(MeetingTime(8, 9))
  }

  def calendarEntries(name: String): IO[List[MeetingTime]] = {
    IO.delay(calendarEntriesApiCall(name))
  }

  def createMeetingApiCall(names: List[String], meetingTime: MeetingTime): Unit = {
    println("start createMeetingApiCall")
  }

  def createMeeting(names: List[String], meetingTime: MeetingTime): IO[Unit] = {
    IO.delay(createMeetingApiCall(names, meetingTime))
  }

  def scheduledMeetings(person1: String, person2: String): IO[List[MeetingTime]] = {
    for {
      person1meetings <- calendarEntries(person1)
      person2meetings <- calendarEntries(person2)
    } yield person1meetings.appendedAll(person2meetings)
  }

  def firstSpace(meetings: List[MeetingTime], lengthHours: Int): Option[MeetingTime] = {
    val spaces = (8 to 16 - lengthHours).toList.map(n => MeetingTime(n, n + lengthHours))
    spaces.find(s => meetings.exists(m => s.start > m.end || s.end < m.start))
  }

  def schedule(person1: String, person2: String, lengthHours: Int): IO[Option[MeetingTime]] = {
    for {
      meetings <- scheduledMeetings(person1, person2)
    } yield firstSpace(meetings, lengthHours)
  }

  def run(): Unit = {
    {
    }
  }
}
