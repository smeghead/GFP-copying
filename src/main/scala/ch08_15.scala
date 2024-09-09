
object Ch08_15 {

  case class MeetingTime (name: String, start: Int, end: Int)


  def calendarEntriesApiCall(name: String): List[MeetingTime] = {
    println("start calendarEntriesApiCall")

    List(MeetingTime("Alice", 8, 9))
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

  def run(): Unit = {
    {
    }
  }
}
