package implicits

import java.util.Date

object JSONSerlialization extends App {

  case class User(name: String, age: Int = 0, email: String = "")
  case class Post(content: String, date: Date)
  case class Feed(user: User, posts: List[Post])

  /* 1
   1. intermidiate data types: Int, String, Date
   2. create type classes for conversion to intermediate data types
   3. serialize to Json
   */

  sealed trait JSONValue {
    def stringify: String
  }

  final case class JSONString(value: String) extends JSONValue {
    override def stringify: String = "\"" + value + "\""
  }

  final case class JSONNumber(value: Int) extends JSONValue {
    override def stringify: String = value.toString
  }

  final case class JSONArray(values: List[JSONValue]) extends JSONValue {
    override def stringify: String = values.map{_.stringify}.mkString("[", ",", "]")
  }

  final case class JSONObject(values: Map[String, JSONValue]) extends JSONValue {
    override def stringify: String = values.map {
      case (key, value) => "\"" + key + ":" + value.stringify
    }.mkString("{", ",", "}")
  }

  val data = JSONObject(Map(
    "user" -> JSONString("Daniel"),
    "posts" -> JSONArray(List(
      JSONString("Scala rocks!"), JSONNumber(42)
    ))
  ))

  println(data.stringify)


  // type classes
  /* 2
   1. type class
   2. type class instances (implicit)
   3. pimp library to do impl conversions
   */
  trait JSONConverter[T] {
    def convert(value: T): JSONValue
  }

  // 2.1 existing data types
  implicit object StringConverter extends JSONConverter[String] {
    override def convert(value: String): JSONValue = JSONString(value)
  }
  implicit object NumberConverter extends JSONConverter[Int] {
    override def convert(value: Int): JSONValue = JSONNumber(value)
  }
  implicit object UserConverter extends JSONConverter[User] {
    override def convert(value: User): JSONValue = JSONObject(Map(
      "name" -> JSONString(value.name),
      "age" -> JSONNumber(value.age),
      "email" -> JSONString(value.email)
    ))
  }

  // 2.3 conversion
  implicit class JSONOps[T](value: T) {
    def toJson(implicit converter: JSONConverter[T]): JSONValue = converter.convert(value)
  }

  // 2.2 custom data types
  implicit object PostConverter extends JSONConverter[Post] {
    override def convert(value: Post): JSONValue = JSONObject(Map(
      "content" -> JSONString(value.content),
      "date" -> JSONString(value.date.toString),
    ))
  }
  implicit object FeedConverter extends JSONConverter[Feed] {
    override def convert(value: Feed): JSONValue = JSONObject(Map(
      "user" -> value.user.toJson,
      "posts" -> JSONArray(value.posts.map {_.toJson})
    ))
  }

  val now = new Date(System.currentTimeMillis())
  val jhon = User("Jhon", 42, "Rock the jvm")
  val feed = Feed(jhon, List(Post("123", now), Post("321", now)))

  // call stringify on result
  println(feed.toJson.stringify)

}
