
import cats.effect.IO
import fs2.{Pure, Stream}
import cats.effect.unsafe.implicits.global
import scala.util.Random
import scala.concurrent.duration._
import cats.implicits.catsSyntaxParallelSequence1

object Ch10_17 {

  def castTheDieImpure(): Int = {
    println("castTheDieImpure")
    val r = new Random
    r.nextInt(6) + 1
  }

  def castTheDie(): IO[Int] = IO.delay(castTheDieImpure())

  def run(): Unit = {
    {
      val result = for {
        _ <- IO.sleep(1.second)
        result <- List(castTheDie(), castTheDie()).parSequence
      } yield result.sum
      println(result.unsafeRunSync())
    }
    {
      val result = for {
        storedCasts <- Ref.of[IO, List[Int]](List.empty)
        singleCast = castTheDie()
                      .flatMap(result => storedCasts.update(_.appended(result)))
        _ <- List(singleCast, singleCast).parSequence
        casts <- storedCasts.get
      } yield casts
      println(result.unsafeRunSync())
    }
    {
      val result = for {
        storedCasts <- Ref.of[IO, List[Int]](List.empty)
        singleCast = castTheDie()
                      .flatMap(result => storedCasts.update(_.appended(result)))
        _ <- List.fill(3)(singleCast).parSequence
        casts <- storedCasts.get
      } yield casts
      println(result.unsafeRunSync())
    }
    {
      val result = for {
        storedCount <- Ref.of[IO, Int](0)
        singleCast = castTheDie()
                      .flatMap(result => storedCount.update(_ + (if (result == 6) result else 0)))
        _ <- List.fill(100)(singleCast).parSequence
        count <- storedCount.get
      } yield count
      println(result.unsafeRunSync())
    }
    {
      val result = List.fill(100)(IO.sleep(1.second).flatMap(_ => castTheDie())).parSequence.map(_.sum)
      println(result.unsafeRunSync())
    }
  }
}