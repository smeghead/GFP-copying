import cats.effect.IO
import fs2.{Pure, Stream}
import cats.effect.unsafe.implicits.global
import scala.util.Random
import scala.concurrent.duration._
import cats.implicits.catsSyntaxParallelSequence1
import org.apache.jena.query._
import org.apache.jena.rdfconnection._

object Ch11_03 {

  opaque type LocationId = String
  object LocationId {
    def apply(value: String): LocationId = value
    extension (a: LocationId) def value: String = a
  }

  case class Location(id: LocationId, name: String, population: Int)
  case class Attraction(name: String, description: Option[String], location: Location)

  enum PopCultureSubject {
    case Artist(name: String, followers: Int)
    case Movie(name: String, boxOffice: Int)
  }
  import PopCultureSubject._

  enum AttractionOrdering {
    case ByName
    case ByLocationPopulation
  }
  import AttractionOrdering._

  case class TravelGuide(attraction: Attraction, subject: List[PopCultureSubject])

  def travelGuide(data: DataAccess, attractionName: String): IO[Option[TravelGuide]] = {
    for {
      attractions <- data.findAtractions(attractionName, ByLocationPopulation, 1)
      guide <- attractions.headOption match {
        case None => IO.pure(None)
        case Some(attraction) =>
          for {
            artists <- data.findArtistsFromLocation(attraction.location.id, 2)
            movies <- data.findMoviesAboutLocation(attraction.location.id, 2)
          } yield Some(TravelGuide(attraction, artists.appendedAll(movies)))
      }
    } yield guide
  }

  trait DataAccess {
    def findArtistsFromLocation(locationId: LocationId, limit: Int): IO[List[Artist]]

    def findMoviesAboutLocation(locationId: LocationId, limit: Int): IO[List[Movie]]

    def findAtractions(name: String, ordering: AttractionOrdering, limit: Int): IO[List[Attraction]]
  }

  val getConnection: IO[RDFConnection] = IO.delay(
    RDFConnectionRemote.create
      .destination("https://query.wikidata.org/")
      .queryEndPoint("sparql").build
  )

  def execQuery(getConnection: IO[RDFConnection], query: String): IO[List[QuerySolution]] = {
    getConnection.flatMap(c => IO.delay(
      asScala(c.query(QueryFactory.create(query)).execSelect()).toList
    ))
  }

  def parseAttraction(s: QuerySolution): IO[Attraction] = {
    IO.delay(Attraction(
      name = s.getLiteral("attractionLabel").getString,
      description =
        if (s.contains("description"))
          Some(s.getLiteral("description").getString)
        else None,
      location = Location(
        id = LocationId(s.getResource("location").getLocalName),
        name = s.getLiteral("locationLabel").getString,
        population = s.getLiteral("population").getInt
      )
    ))
  }

  def findAtractions(name: String, ordering: AttractionOrdering, limit: Int): IO[List[Attraction]] = {
    val orderBy = ordering match {
      case ByName => "?attractionLabel"
      case ByLocationPopulation => "DESC(?population)"
    }

    val query = s"""... SELECT DISTINCT ?attraction ?attractionLabel
      ?description ?location ?locationLabel ?population WHERE {
        ...
      } ORDER BY $orderBy LIMIT $limit"""

    for {
      solutions <- execQuery(getConnection, query)
      attractions <- solutions.traverse(parseAttraction)
    } yield attractions
  }

  def run(): Unit = {
    {
    }
  }
}