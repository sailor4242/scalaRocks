package concurrency

import scala.concurrent.{Await, ExecutionContext, Future, Promise}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.util.{Failure, Random, Success, Try}

object FuturePromises extends App {

  def calc: Int = {
    Thread.sleep(1000)
    42
  }

  val aFuture = Future {
    calc // runs this using apply on another thread
  } // global passed by the compiler

  println(aFuture.value) // Option[Try[Int]]

  println("Waiting")
  aFuture.onComplete {
    case Success(m) => println(s"calculated $m")
    case Failure(e) => println(s"failed with $e")
  }
  Thread.sleep(3000)

  // mini social network

  case class Profile(id: String, name: String) {
    def poke(anotherProfile: Profile): Unit = {
      println(s"${this.name} poking ${anotherProfile.name}")
    }
  }

  object SocialNetwork {
    //database
    val names = Map(
      "fb.id.1-zuck" -> "Mark",
      "fb.id.2-bill" -> "Bill",
      "fb.id.0-dummy" -> "Dummy",
    )

    val friends = Map(
      "fb.id.1-zuck" -> "fb.id.2-bill"
    )
    val random = new Random()

    //API
    def fetchProfile(id: String): Future[Profile] = Future {
      Thread.sleep(random.nextInt(300))
      Profile(id, names(id))
    }
    def fetchBestFriend(profile: Profile): Future[Profile] = Future {
      Thread.sleep(random.nextInt(400))
      val bfId = friends(profile.id)
      Profile(bfId, names(bfId))
    }
  }

  // client: mark poke bill
  val mark = SocialNetwork.fetchProfile("fb.id.1-zuck")
  mark.onComplete({
    case Success(mp) => {
      val bill = SocialNetwork.fetchBestFriend(mp)
      bill.onComplete({
        case Success(bp) => mp.poke(bp)
        case Failure(e) => println(e.getStackTrace)
      })
    }
    case Failure(e) => println(e.getStackTrace)
  })
  Thread.sleep(1000)

  // functional composition
  // map, flatMap, filter
  val nameOnTheWall = mark.map(profile => profile.name)
  val markBestFriend = mark.flatMap(profile => SocialNetwork.fetchBestFriend(profile))
  val zucksBestFriendRestricted = markBestFriend.filter(profile => profile.name.startsWith("2"))

  for {
    mark <- SocialNetwork.fetchProfile("fb.id.1-zuck")
    bill <- SocialNetwork.fetchBestFriend(mark)

  } yield mark.poke(bill)

  Thread.sleep(1000)

  //fallbacks
  val adummy = SocialNetwork.fetchProfile("unknown id").recover {
    case e: Throwable => Profile("fb.id.0-dummy", "Dummy")
  }

  val adummyWith = SocialNetwork.fetchProfile("unknown id").recoverWith {
    case e: Throwable => SocialNetwork.fetchProfile("fb.id.0-dummy")
  }

  val fallBack = SocialNetwork.fetchProfile("unknown id").fallbackTo(SocialNetwork.fetchProfile("fb.id.0-dummy"))

  // blocking on futures

  case class User(name: String)
  case class Transaction(sender: String, receiver: String, amount: Double, status: String)

  object BankingApp {
    val name = "Rock the JVM banking"

    def fetchUser(name: String): Future[User] = Future {
      //simuate fetch from DB
      Thread.sleep(500)
      User(name)
    }

    def createTransaction(user: User, merchantName: String, amount: Double): Future[Transaction] = Future {
      // simulate some process
      Thread.sleep(1000)
      Transaction(user.name, merchantName, amount, "SUCCESS")
    }

    def purchase(username: String, item: String, merchant: String, cost: Double): String = {
      // fetch user from db
      // create tr
      // wait tr to finish
      val trStatus = for {
        user <- fetchUser(username)
        tr <- createTransaction(user, merchant, cost)
      } yield tr.status

      Await.result(trStatus, 2.seconds) // implicit conversions -> pimp my library
    }
  }

  println(BankingApp.purchase(username = "Vasya", item = "IphoneXX", merchant = "Store", cost = 3000))

  // promises

  val promise = Promise[Int]() // "controller" over future
  val future = promise.future

  // producer-consumer with promises

  //thread 1 - consumer
  future.onComplete {
    case Success(r) => println(s"[consumer] i've recieved $r")
  }

  //thread 2 - producer
  val producer = new Thread(() => {
    println(s"[producer] crunching")
    Thread.sleep(500)
    promise.success(42)
    println("[producer] done")
  })

  producer.start()
  Thread.sleep(1000)

  // EXERCISES
  /*
    1) fulfill a future IMMEDIATELY with a value
    2) inSequence(fa, fb) fb runs AFTER fa is completed
    3) first(fa, fb) = > new future with the first value of the two
    4) same but with the last value
    5) retryUntil(action: () => Future[T], condition: T => Boolean): Future[T]
   */

  // 1
  def immediately[T](v: T) = Future( v)

  //2
  def inSequence[T](fa: Future[T], fb: Future[T]) = {
    for {
      a <- fa
      b <- fb
    } yield (a, b)
  }

  //3
  def first[T](fa: Future[T], fb: Future[T]): Unit = {
    val a = fa.isCompleted
    val b = fb.isCompleted

    while(true) {
      if (a) return fa.value.get.get
      else if (b) return fb.value.get.get
    }
  }

  def First2[T](fa: Future[T], fb: Future[T]): Future[T] = {
    val promise = Promise[T]
    fa.onComplete(promise.tryComplete)
    fb.onComplete(promise.tryComplete)

    promise.future
  }

  //4
  def last[T](fa: Future[T], fb: Future[T]): Future[T] = {
    val promiseFirst = Promise[T]
    val promiseLast = Promise[T]
    val check = (r: Try[T]) => if (!promiseFirst.tryComplete(r)) promiseLast.complete(r)
    fa.onComplete(check)
    fb.onComplete(check)
    promiseLast.future
  }

  //5
  def retryUntil[T](action: () => Future[T], condition: T => Boolean): Future[T] = {
    action().filter(condition).recoverWith {
      case _ => retryUntil(action, condition)
    }
  }

}
