package implicits

import scala.concurrent.Future

object MagnetPattern extends App {

  import scala.concurrent.ExecutionContext.Implicits.global._
  // method overloading
  class P2PRequest
  class P2PResponse
  trait Serializer[T]
  trait Actor {
    // lots of overloads
    def receive(statisCode: Int): Int
    def receive(request: P2PRequest): P2PResponse
    def receive(response: P2PResponse): Int
    def receive[T : Serializer](message: T): Int
    def receive[T : Serializer](message: T, statusCode: Int): Int
    def receive(future: Future[P2PRequest]): Int
    // def receive(future: Future[P2PResponse]): Int // not compile because of type erasure
    /* cons:
     type erasure
     code duplication
     lifting doesn't work for all overloads
       val a = receive () _ ?!
     no type inference for default args
        a.receive()
    */
  }


  trait MessageMagnet[Result]{
    def apply(): Result
  }

  def receive[R](magnet: MessageMagnet[R]): R = magnet()

  implicit class FromP2PRequest(request: P2PRequest) extends MessageMagnet[Int] {
    override def apply(): Int = {
      // logic for handling request
      42
    }
  }
  implicit class FromP2PResponse(response: P2PResponse) extends MessageMagnet[Int] {
    override def apply(): Int = {
      // logic for handling response
      24
    }
  }
  receive(new P2PRequest)
  receive(new P2PResponse)

  implicit class FutureFromP2PRequest(response: Future[P2PRequest]) extends MessageMagnet[Int] {
    override def apply(): Int = {
      // logic for handling response
     31
    }
  }
  implicit class FutureFromP2PResponse(response: Future[P2PResponse]) extends MessageMagnet[Int] {
    override def apply(): Int = {
      // logic for handling response
      13
    }
  }
  // now compiles! no type erasure problem
  //receive(Future[P2PRequest]{new P2PRequest})
  //receive(Future[P2PResponse]{new P2PResponse})



  // lifting works
  trait MathLib {
    def +++(x: Int):Int
  }
  trait MathMagnet {
    def apply(): Int
  }

  def +++(magnet: MathMagnet): Int = magnet()

  implicit class AddInt(v: Int) extends MathMagnet  {
    override def apply: Int = v + 1
  }

  val add = +++_
  add(3)

  // Exercise
  // still call by name doesn't work

}
