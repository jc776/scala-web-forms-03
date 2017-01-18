package example.server

object Main {
	def main(args: Array[String]): Unit = {
		//Decoders.run()
		
	
		AkkaServer.run()
	}
}

case class Parent(child1: Child, child2: Child)
case class Child(num: Int, str: String)
case class Multi(parent: Parent, child3: Child)

object AkkaServer {
	import akka.actor.ActorSystem
	import akka.stream.ActorMaterializer
	
	import akka.http.scaladsl.Http
	import akka.http.scaladsl.model._
	import akka.http.scaladsl.server.Directives.{method => serverMethod, head => serverHead, _}
	
	import scalatags.Text.all._
	
	implicit val system = ActorSystem("my-system")
    implicit val materializer = ActorMaterializer()
    implicit val executionContext = system.dispatcher // does this work on regular Futures?
	
	import library.server.FormDecoder
	import library.server.FormDecoder._
	
	import library.server.Editor
	
	def editForm[T](item: Option[T])(implicit editT: Editor[T]) = form(
		method := "post",
		`class` := "form-horizontal",
		editT("", item),
		button(`type` := "submit", "Send")
	)
	
	def tableEditor[T: FormDecoder: Editor] = {
		get {
			htmlResponse(editForm(None))
		} ~
		post {
			entity(as[T]) { value =>
				htmlResponse(div(h2("Data!"), pre(value.toString), editForm(Some(value))))
			} ~
			htmlResponse(pre("Couldn't read the form."))
		}
	}
	
	def template(content: Frag) = html(
		head(
			link(rel := "stylesheet", href := "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.7/css/bootstrap.min.css")
		),
		body(div(`class` := "container", content))
	)
	
	def htmlResponse(html: Frag) = complete(HttpEntity(ContentTypes.`text/html(UTF-8)`, template(html).render))
	// jsonResponse
	// etc etc
	
	val route =
      pathPrefix("parent") { tableEditor[Parent] } ~
	  pathPrefix("child") { tableEditor[Child] } ~
	  pathPrefix("multi") { tableEditor[Multi] } ~
      htmlResponse(pre("404"))

	def run() {
		val host = "localhost"
		val port = 12355
		val binding = Http().bindAndHandle(route, host, port)
		binding.onFailure { case ex: Exception =>
			System.err.println(s"Failed to bind to http://$host:$port!", ex)
		}
		println(s"OK, started at http://$host:$port")
		sys.addShutdownHook(system.terminate())
	}
}

object Decoders {
	import library.server.FormDecoder
	
	import akka.actor.ActorSystem
	import akka.stream.ActorMaterializer
	
	import akka.http.scaladsl.unmarshalling.Unmarshal
	import akka.http.scaladsl.common.StrictForm
	import akka.http.scaladsl.model.FormData
	
	import scala.concurrent.Await
	import scala.concurrent.duration.Duration
	
	implicit val system = ActorSystem("my-system")
    implicit val materializer = ActorMaterializer()
    implicit val executionContext = system.dispatcher // does this work on regular Futures?
	
	def run(){
		val parentInput = FormData(Map(
			"child1.num" -> "1", 
			"child1.str" -> "a",
			"child2.num" -> "2", 
			"child2.str" -> "b"
		)).toEntity
		val childInput = FormData(Map(
			"num" -> "1", 
			"str" -> "a"
		)).toEntity
		val ft = Unmarshal(parentInput).to[StrictForm].flatMap(FormDecoder.decode[Parent] _)
		
		val result = Await.result(ft, Duration.Inf)
		val expected = Parent(child1 = Child(1, "a"), child2 = Child(2, "b"))
		println(result)
		assert(result == expected)
		
		//List[Child]
	}

}

case class User(name: String, id: Option[Int] = None)

object SlickDemo {
	import scala.concurrent.Await
	import scala.concurrent.ExecutionContext.Implicits.global
	import scala.concurrent.duration.Duration

	import slick.jdbc.H2Profile.api._

	class Users(tag: Tag) extends Table[User](tag, "USERS") {
		def id = column[Int]("ID", O.PrimaryKey, O.AutoInc)
		def name = column[String]("NAME")
		
		// the * projection (e.g. select * ...) auto-transforms the tupled
		// column values to / from a User
		def * = (name, id.?) <> (User.tupled, User.unapply)
	}

	def run() {
		val users = TableQuery[Users]
		
		println("Connecting to DB.")
		val db = Database.forURL("jdbc:h2:mem:test1;DB_CLOSE_DELAY=-1", driver="org.h2.Driver")
		try {
			Await.result(db.run(DBIO.seq(
			  // create the schema
			  users.schema.create,

			  // insert two User instances
			  users += User("John Doe"),
			  users += User("Fred Smith"),

			  // print the users (select * from USERS)
			  users.result.map(println)
			)), Duration.Inf)
		} finally db.close
	}
}