object Ch06_03 {

  case class TvShow(name: String, start: Int, end: Int)

  def sortShows(shows: List[TvShow]): List[TvShow] = {
    shows
      .sortBy(tvShow => tvShow.end - tvShow.start)
      .reverse
  }

  def parseShows(rawShows: List[String]): Either[String, List[TvShow]] = {
    val initialResult: Either[String, List[TvShow]] = Right(List.empty)

    rawShows
      .map(parseShow)
      .foldLeft(initialResult)(addOrResign)
  }

  def extractName(rawShow: String): Either[String, String] = {
    val bracketOpen = rawShow.indexOf('(')
    if (bracketOpen != -1)
      Right(rawShow.substring(0, bracketOpen).trim)
    else Left(s"Can't extract name from $rawShow")
  }

  def extractYearEnd(rawShow: String): Either[String, Int] = {
    val bracketClose = rawShow.indexOf(')')
    val dash = rawShow.indexOf('-')
    
    for {
      yearStr <- if (dash != -1 && bracketClose > dash + 1)
                   Right(rawShow.substring(dash + 1, bracketClose))
                 else Left(s"Can't extract end year from $rawShow")
      year <- yearStr.toIntOption.toRight(s"Can't parse $yearStr")
    } yield year
  }

  def extractYearStart(rawShow: String): Either[String, Int] = {
    val bracketOpen = rawShow.indexOf('(')
    val dash = rawShow.indexOf('-')
    for {
      yearStr <- if (bracketOpen != -1 && dash > bracketOpen + 1)
                   Right(rawShow.substring(bracketOpen + 1, dash))
                 else Left(s"Can't extract start year from $rawShow")
      year <- yearStr.toIntOption.toRight(s"Can't parse $yearStr")
    } yield year
  }

  def extractSingleYear(rawShow: String): Either[String, Int] = {
    val bracketOpen = rawShow.indexOf('(')
    val bracketClose = rawShow.indexOf(')')
    val dash = rawShow.indexOf('-')
    for {
      yearStr <- if (dash == -1 && bracketOpen != -1 && bracketClose > bracketOpen + 1)
                   Right(rawShow.substring(bracketOpen + 1, bracketClose))
                 else Left(s"Can't extract single year from $rawShow")
      year <- yearStr.toIntOption.toRight(s"Can't parse $yearStr")
    } yield year
  }

  def parseShow(rawShow: String): Either[String, TvShow] = {
    for {
      name <- extractName(rawShow)
      yearStart <- extractYearStart(rawShow).orElse(extractSingleYear(rawShow))
      yearEnd <- extractYearEnd(rawShow).orElse(extractSingleYear(rawShow))
    } yield TvShow(name, yearStart, yearEnd)
  }

  def addOrResign(
    parsedShows: Either[String, List[TvShow]],
    newParsedShow: Either[String, TvShow]
  ): Either[String, List[TvShow]] = {
    for {
      shows <- parsedShows
      newShow <- newParsedShow
    } yield shows.appended(newShow)
  }

  val shows = List(TvShow("Breaking Bad", 2008, 2013),
                    TvShow("The Wire", 2002, 2008),
                    TvShow("Mad Men", 2008, 2015))

  val rawShows = List("Breaking Bad (2008-2013)",
                      "The Wire 2002 2008",
                      "Mad Men (2007-2015)")

  def run(): Unit = {
    {
      println(sortShows(shows))
    }
    {
      println(parseShow("Braking Bad (2008-2013)"))
    }
    {
      println(parseShow("Chernobyl (2019)"))
    }
    {
      println(parseShows(rawShows))
    }
  }
}
