package library.server

import akka.http.scaladsl.common.StrictForm
import akka.stream.Materializer

import scala.concurrent.{ ExecutionContext, Future }

import akka.http.scaladsl.unmarshalling._
import akka.http.scaladsl.model._

// - I can run a server
// - I can connect to a database
// - I can write HTML
// - I can take apart a type with Shapeless
// - I can change results per type of field with LabelledGeneric & Type-Classes

// editor[Id[Person]]
// editor[SpecialOrganizationId] - uses Editor[Id[Org]], somehow.
// also, Id[X] should retrieve by *search*, not 

// val route = path("person") { tableEditorForms[Person] }

// can akka-http read forms as case-classes? s. might be able to do a Map.

trait FormDecoder[T] {
	// to do... remove the Akka from this so I've got a clean one to use with my own HTML
	// but the same method is handy for a Map[String,String]
	
	def apply(prefix: String, form: StrictForm)(implicit ec: ExecutionContext, mat: Materializer): Future[T] // or Result/Future
}

object FormDecoder {
	import shapeless.{ ::, :+:, CNil, Coproduct, HList, HNil, Inl, Inr, LabelledGeneric, Lazy, Witness }
	import shapeless.labelled.{ FieldType, field }
	
	import akka.http.scaladsl.unmarshalling._
	
	implicit def decodeSingleField[T](implicit 
		uf: FromStrictFormFieldUnmarshaller[T]
	): FormDecoder[T] = new FormDecoder[T] {
		def apply(prefix: String, form: StrictForm)(implicit ec: ExecutionContext, mat: Materializer) = {
			//println("Tried field " + prefix)
			uf(form.field(prefix))
		}
	}
	
	def decoder[T](implicit dec: FormDecoder[T]) = dec
	
	def decode[T](form: StrictForm)(implicit dec: FormDecoder[T], ec: ExecutionContext, mat: Materializer) = dec("", form)
	
	implicit def unmarshaller[T](implicit dec: FormDecoder[T]): FromRequestUnmarshaller[T] = 
		Unmarshaller.withMaterializer(implicit ec => implicit mat => req => {
			Unmarshal(req.entity).to[StrictForm].flatMap(form => {
				println("FORM: " + form.fields.toString)
				dec.apply("", form)
			})
		})
	
	
	// Why does Circe have two levels of decoders? Can they be overridden?
	
	implicit val decodeHNil = new FormDecoder[HNil] {
		def apply(prefix: String, form: StrictForm)(implicit ec: ExecutionContext, mat: Materializer) = Future.successful(HNil)
	}
	
	def childPrefix(prefix: String, name: String) = {
		if(prefix == "")
			name
		else
			s"${prefix}.${name}"
	}
	
	implicit def decodeHCons[K <: Symbol, H, T <: HList](implicit
		key: Witness.Aux[K],                     // If K has a readable name
		decodeH: Lazy[FormDecoder[H]],           // and I can decode type H
		decodeT: Lazy[FormDecoder[T]]            // and I can decode the rest of the list
	): FormDecoder[FieldType[K, H] :: T]      
	= new FormDecoder[FieldType[K, H] :: T] {    // then I can decode a record {name: H, ...T}
		def apply(prefix: String, form: StrictForm)(implicit ec: ExecutionContext, mat: Materializer) = {
			for {
				h <- decodeH.value(childPrefix(prefix, key.value.name), form)
				t <- decodeT.value(prefix, form)         
			} yield field[K](h) :: t
		}
	}
	
	implicit def decodeCaseClass[A, R <: HList](implicit
       lgen: LabelledGeneric.Aux[A, R], // If I can split case-class A into a record R
       decodeR: Lazy[FormDecoder[R]]    // and I can read an R from a form
	) = new FormDecoder[A] {            // then I can read an A from a form
		def apply(prefix: String, form: StrictForm)(implicit ec: ExecutionContext, mat: Materializer) = {
			decodeR.value(prefix, form).map(lgen.from)        
		}
	}
	
	//implicit def decodeCoproduct[K <: Symbol, L, R <: Coproduct] - Not sure how to represent this.
	//implicit val decodeCNil: DerivedDecoder[CNil]
	//implicit def decodeAdt[A, R <: Coproduct]
	
	
}


trait Editor[T] {
	def apply(prefix: String, item: Option[T]): scalatags.Text.all.Frag
}

object Editor {
	import shapeless.{ ::, :+:, CNil, Coproduct, HList, HNil, Inl, Inr, LabelledGeneric, Lazy, Witness }
	import shapeless.labelled.{ FieldType, field }
	
	import scalatags.Text.all._
	
	implicit val editInt = new Editor[Int] {
		def apply(prefix: String, item: Option[Int]) = div(input(name := prefix, item.map(value := _)), "[NUMBER]")
	}

	implicit val editString = new Editor[String] {
		def apply(prefix: String, item: Option[String]) = input(name := prefix, item.map(value := _))
	}
	
	implicit val editHNil = new Editor[HNil] {
		def apply(prefix: String, item: Option[HNil]) = List[Frag]()
	}
	
	implicit def decodeHCons[K <: Symbol, H, T <: HList](implicit
		key: Witness.Aux[K],               // If K has a readable name
		editH: Lazy[Editor[H]],            // and I have an editor for the head's item
		editT: Lazy[Editor[T]]             // and I have an editor for the rest of the list
	) = new Editor[FieldType[K, H] :: T] { // then I can make an editor for the whole list.
		def apply(prefix: String, item: Option[FieldType[K, H] :: T]) = {
			val childPrefix = FormDecoder.childPrefix(prefix, key.value.name)
			item match {
				case Some(hl) => List[Frag](
					div(`class` := "form-group")(
						label(`class` := "col-sm-2", key.value.name, ":"), 
						div(`class` := "col-sm-10", editH.value(childPrefix, Some(hl.head)))
					),
					editT.value(prefix, Some(hl.tail))
				)
				case None => List[Frag](
					// even without an input, you still need an editor for it.
					div(`class` := "form-group")(
						label(`class` := "col-sm-2", key.value.name, ":"), 
						div(`class` := "col-sm-10", editH.value(childPrefix, None))
					),
					editT.value(prefix, None)
				)
			}
		}
	}  
	
	implicit def editCaseClass[A, R <: HList](implicit
       lgen: LabelledGeneric.Aux[A, R], // If   I can split case-class A into a record R
       editR: Lazy[Editor[R]]           // and  I can edit that record
	) = new Editor[A] {                 // then I can edit an A
		def apply(prefix: String, item: Option[A]) = editR.value(prefix, item.map(lgen.to))
	}
}