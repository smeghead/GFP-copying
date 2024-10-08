
import cats.effect.{IO, Ref}
import fs2.{Pure, Stream}
import cats.effect.unsafe.implicits.global

object Ch10_02 {

  opaque type City = String
  object City {
    def apply(name: String): City = name
    extension (city: City) def name: String = city
  }

  case class CityStats(city: City, checkIns: Int)

  def processCheckIns(checkIns: Stream[IO, City]): IO[Unit] = {
    checkIns
      .scan(Map.empty[City, Int])((cityCheckIns, city) =>
        cityCheckIns
          .updatedWith(city)(_.map(_ + 1).orElse(Some(1)))
      )
      .chunkN(100_000)
      .map(_.last)
      .unNone
      .map(topCities)
      .foreach(IO.println)
      .compile.drain
  }

  def topCities(cityCheckIns: Map[City, Int]): List[CityStats] = {
    cityCheckIns.toList
      .map(_ match {
        case (city, checkIns) => CityStats(city, checkIns)
      })
      .sortBy(_.checkIns)
      .reverse
      .take(3)
  }

  // val checkIns: Stream[IO, City] =
  //   Stream(
  //     City("Sydney"),
  //     City("Sydney"),
  //     City("Cape Town"),
  //     City("Singapore"),
  //     City("Cape Town"),
  //     City("Sydney"),
  //   ).covary[IO]
  val checkIns: Stream[IO, City] =
    Stream(
      City("Sydney"),
      City("Dublin"),
      City("Cape Town"),
      City("Lima"),
      City("Singapore")
    )
    .repeatN(100_000)
    .append(Stream.range(0, 100_000).map(i => City(s"City $i")))
    .append(Stream(City("Sydney"), City("Sydney"), City("Lima")))
    .covary[IO]

  def run(): Unit = {
    {
      processCheckIns(checkIns).unsafeRunSync()
    }
  }
}
