package example

import cats.effect._
import cats.implicits._
import scala.concurrent.Future

// IO.asyncやIO.shiftの効果を調べたい

object Hello extends IOApp.Simple with AsynchronousComputation with Download {
  import scala.concurrent.duration._
  import scala.language.postfixOps
  val cpuCount = IO(Runtime.getRuntime().availableProcessors())

  val run = for {
    cnt <- cpuCount
    _ <- IO.println(s"Available #CPU: ${cnt}")
    _ <- IO.println("Running tremendous jobs synchronously...")
    _ <- IO.sleep(1 second)
    _ <- clearScreen
    // Compute用のThread pool[io-compute-*]が利用される
    // ThreadはCore数いっぱい生成され、使いまわされる
    _ <- List.range(0, 2500).parTraverse(n => doSomething(n))
    _ <- IO.println("Now, we are going to compute on pool for blocking I/O...")
    _ <- clearScreen
    _ <- List
      .range(0, 2500)
      .parTraverse(i =>
        IO.blocking {
          // println(s"(inside blocking)[${Thread.currentThread().getName()}]")
          val y = i / 50
          val x = i % 50
          print(s"\u001b[$y;${x}H*")
          Thread.sleep((i * 10).toLong)
          print(s"\u001b[$y;${x}H${threadColor}")
          i
        }
      ) // debugはここでは使えない(debug自体はblockingの外で実行されるため)
    _ <- IO.println("CE3 doesn't have IO.shift. but it has IO.blocking()")
    _ <- IO("one").debug
    _ <- IO("two").debug
    _ <- IO
      .blocking(s"blocking three: ${Thread.currentThread().getName()}")
      .debug
    _ <- IO("four").debug
    _ <- IO("five").debug
    _ <- IO("six").debug
    // _ <- IO.fromFuture(IO(k1))
    // _ <- IO.fromFuture(IO(k1))
    downloadedFile <- backgroundDownloadingIndicator.use { _ =>
      verySlowDownload.debug
    }
    _ <- IO.println(s"we have downloaded $downloadedFile")
  } yield IO.unit

}

trait AsynchronousComputation {
  import scala.concurrent.duration._
  import scala.language.postfixOps
  implicit val ec: scala.concurrent.ExecutionContext =
    scala.concurrent.ExecutionContext.global
  def k1 = Future {
    Thread.sleep(1000);
    println(s"k1 is running on ${Thread.currentThread().getName()}")
  }

  def clearScreen = IO.print("\u001b[2J")
  def threadColor = {
    val n = Thread.currentThread().getId() % 32
    val r = n * 8
    s"\u001b[48;2;$r;0;0m \u001b[0m"
  }
  def doSomething(i: Int) = {
    val y = i / 50
    val x = i % 50
    IO.print(s"\u001b[$y;${x}H*") >> IO.sleep(i / 100 second) >> IO {
      print(s"\u001b[$y;${x}H${threadColor}")
    }
  }

  implicit class IOShow[A](io: IO[A]) {
    def debug(): IO[A] = for {
      res <- io
      _ <- IO.println(s"[${Thread.currentThread().getName()}] -> ${res}")
    } yield res
  }
}

trait Download {
  import scala.concurrent.duration._
  import scala.language.postfixOps

  def verySlowDownload: IO[String] =
    IO.println("Starting Download") >> IO.sleep(3 second) >> IO.blocking {
      // Finally we got result
      "/tmp/foobar.txt"
    }

  def backgroundDownloadingIndicator: ResourceIO[IO[OutcomeIO[Unit]]] =
    downloadingIndicator.background
  def downloadingIndicator: IO[Unit] =
    IO.sleep(100 milliseconds) *> (IO.print("\r|") *> IO.sleep(
      100 milliseconds
    ) *> IO.print("\r/") *> IO.sleep(100 milliseconds) *> IO.print("\r-") *> IO
      .sleep(100 milliseconds) *> IO.print("\r\\") *> IO.sleep(
      100 milliseconds
    )).foreverM
}
