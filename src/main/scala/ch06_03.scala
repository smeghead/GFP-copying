object Ch06_03 {

  case class TvShow(name: String, start: Int, end: Int)

  def sortShows(shows: List[TvShow]): List[TvShow] = {
    shows
      .sortBy(tvShow => tvShow.end - tvShow.start)
      .reverse
  }

  def extractName(rawShow: String): Option[String] = {
    val bracketOpen = rawShow.indexOf('(')
    if (bracketOpen != -1)
      Some(rawShow.substring(0, bracketOpen).trim)
    else None
  }

  def extractYearEnd(rawShow: String): Option[Int] = {
    val bracketClose = rawShow.indexOf(')')
    val dash = rawShow.indexOf('-')
    
    for {
      yearStr <- if (dash != -1 && bracketClose > dash + 1)
                   Some(rawShow.substring(dash + 1, bracketClose))
                 else None
      year <- yearStr.toIntOption
    } yield year
  }

  def extractYearStart(rawShow: String): Option[Int] = {
    val bracketOpen = rawShow.indexOf('(')
    val dash = rawShow.indexOf('-')
    for {
      yearStr <- if (bracketOpen != -1 && dash > bracketOpen + 1)
                   Some(rawShow.substring(bracketOpen + 1, dash))
                 else None
      year <- yearStr.toIntOption
    } yield year
  }

  def parseShow(rawShow: String): Option[TvShow] = {
    for {
      name <- extractName(rawShow)
      yearStart <- extractYearStart(rawShow)
      yearEnd <- extractYearEnd(rawShow)
    } yield TvShow(name, yearStart, yearEnd)

    // val bracketOpen = rawShow.indexOf('(')
    // val bracketClose = rawShow.indexOf(')')
    // val dash = rawShow.indexOf('-')

    // val name = rawShow.substring(0, bracketOpen).trim
    // val yearStart = Integer.parseInt(rawShow.substring(bracketOpen + 1, dash))
    // val yearEnd = Integer.parseInt(rawShow.substring(dash + 1, bracketClose))

    // TvShow(name, yearStart, yearEnd)
  }

  val shows = List(TvShow("Breaking Bad", 2008, 2013),
                    TvShow("The Wire", 2002, 2008),
                    TvShow("Mad Men", 2008, 2015))

  def run(): Unit = {
    {
      println(sortShows(shows))
    }
    {
      println(parseShow("Braking Bad (2008-2013)"))
    }
  }
}
