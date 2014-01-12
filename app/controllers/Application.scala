//play service
package controllers

import play.api.libs.functional.syntax._
import play.api._
import play.api.mvc._
import play.api.libs.json._
import play.api.http.HttpProtocol
import play.api.libs.functional.syntax._
import scala.util.parsing.json.JSONObject
import play.api.db._
import java.io._
import scala.io.Source
import models.UserReturn


object Application extends Controller {

  
  def index = Action {
    Ok("This is the index page")
  }
  
  //reads a file of users
  def readFile (name:String, id:String) : JsValue =
  {
	var jsonVal= Json.parse("""{"name": "no data", "id":"000", "age": 0, "gender":"male","address":"n/a" }""")
    
    for(line <- Source.fromFile("custData.txt").getLines){
    	jsonVal=Json.parse(line)
    	
    	val foundMatch: Boolean=jsonVal.validate[UserReturn] match 
    	{
    	   case JsSuccess(user, _) => user.name.equals(name) && user.id.equals(id)
    	}
    	
    	if(foundMatch)
    	{
    	  return jsonVal
    	}
    }
	jsonVal= Json.parse("""{"name": "no data","id":"000", "age": 0, "gender":"n/a","address":"n/a" }""")
    jsonVal
   
  }
  
  //How to read a user return object
     implicit val userReturnReads: Reads[UserReturn] = (
	   (__ \ "name").read[String]
	   and (__ \ "id").read[String]
		and (__ \ "age").read[Int] 
		and (__ \ "gender").read[String] 
	   and (__ \ "address").read[String] 
	 )(UserReturn)
 
  //How to read the json through validate
  implicit val rds = (
    (__ \ 'name).read[String] and
    (__ \ 'age).read[String]
   ) tupled
   

  
   //Sends back a json response
   def jsonRespond = Action(parse.json)   {request =>
   request.body.validate[(String, String)].map{ 
   case (name, id) => 
    val furtherInfo: JsValue=readFile(name,id) 
    
   	//val buildJson: JsObject= Json.obj("name" -> name,"age" -> 25, "gender" -> "male", "address" -> "14 Fake Street, Fake Town, NE72 PAS")
    Ok(Json.toJson(furtherInfo))  
    }.recoverTotal{
      e => BadRequest(Json.obj("status" ->"KO", "message" -> JsError.toFlatJson(e)))
    }   
  }



}