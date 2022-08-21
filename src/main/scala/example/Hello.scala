package example

import cats.effect._
import cats.implicits._
import scala.concurrent.Future

// IO.asyncやIO.shiftの効果を調べたい

object Hello extends IOApp.Simple with AsynchronousComputation with Download {
  val cpuCount = IO(Runtime.getRuntime().availableProcessors())

  val run = for {
    // cnt <- cpuCount
    // _ <- IO.println(s"Available #CPU: ${cnt}")
    // _ <- IO.println("Running tremendous jobs synchronously...")
    // // Compute用のThread pool[io-compute-*]が利用される
    // // ThreadはCore数いっぱい生成され、使いまわされる
    // _ <- List.range(0, cnt * 2).parTraverse(n => doSomething(n).debug)
    // _ <- IO.println("Now, we are going to compute on pool for blocking I/O...")
    // _ <- List
    //   .range(0, cnt * 2)
    //   .parTraverse(n =>
    //     IO.blocking {
    //       Thread.sleep((Math.random() * 1000).toLong)
    //       println(s"(inside blocking)[${Thread.currentThread().getName()}]")
    //       n
    //     }
    //   ) // debugはここでは使えない(debug自体はblockingの外で実行されるため)
    // _ <- IO.println("CE3 doesn't have IO.shift. but it has IO.blocking()")
    // _ <- IO("one").debug
    // _ <- IO("two").debug
    // _ <- IO
    //   .blocking(s"blocking three: ${Thread.currentThread().getName()}")
    //   .debug
    // _ <- IO("four").debug
    // _ <- IO("five").debug
    // _ <- IO("six").debug
    // _ <- IO.fromFuture(IO(k1))
    // _ <- IO.fromFuture(IO(k1))
    downloadedFile <- backgroundDownloadingIndicator.use { _ => verySlowDownload.debug }
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

  def doSomething(i: Int) =
    IO.sleep(Math.random() * 10 second) >> IO(i)

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

  def verySlowDownload: IO[String] = IO.println("Starting Download") >> IO.sleep(3 second) >> IO.delay {
    // Finally we got result
    "/tmp/foobar.txt"
  }

  def backgroundDownloadingIndicator: ResourceIO[IO[OutcomeIO[Unit]]] = downloadingIndicator.background
  def downloadingIndicator: IO[Unit] =
    IO.sleep(100 milliseconds) *> (IO.print("\r|") *> IO.sleep(100 milliseconds) *> IO.print("\r/") *> IO.sleep(100 milliseconds) *> IO.print("\r-") *> IO.sleep(100 milliseconds) *> IO.print("\r\\") *> IO.sleep(100 milliseconds)).foreverM
}
