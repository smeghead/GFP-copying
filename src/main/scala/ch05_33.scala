object Ch05_33 {

  case class Event(name: String, start: Int, end: Int)

  def validateName(name: String): Option[String] =
    if (name.size > 0) Some(name) else None

  def validEnd(end: Int): Option[Int] =
    if (end < 3000) Some(end) else None

  def validStart(start: Int, end: Int): Option[Int] =
    if (start <= end) Some(start) else None

  def validLength(start: Int, end: Int, minLength: Int): Option[Int] = {
    val term = end - start
    if (term >= minLength) Some(term) else None
  }

  def parse(name: String, start: Int, end: Int): Option[Event] =
    for {
      validName <- validateName(name)
      validEnd <- validEnd(end)
      validStart <- validStart(start, end)
    } yield Event(validName, validStart, validEnd)

  def parseLongEvent(name: String, start: Int, end: Int, minLength: Int): Option[Event] =
    for {
      validName <- validateName(name)
      validEnd <- validEnd(end)
      validStart <- validStart(start, end)
      validLength <- validLength(start, end, minLength)
    } yield Event(validName, validStart, validEnd)

  def run(): Unit = {
    {
      val e = parse("Apollo Program", 1961, 1972)
      println(e)
      assert(e == Some(Event("Apollo Program", 1961, 1972)))
    }
    {
      val e = parse("", 1961, 1972)
      println(e)
      assert(e == None)
    }
    {
      val e = parseLongEvent("Apollo Program", 1961, 1972, 10)
      println(e)
      assert(e == Some(Event("Apollo Program", 1961, 1972)))
    }
    {
      val e = parseLongEvent("Apollo Program", 1961, 1970, 10)
      println(e)
      assert(e == None)
    }
  }
}
