package org.am.scala.essentials.helper

import org.scalatest.WordSpec
import org.scalatest.matchers.ShouldMatchers
import java.net.URL
import org.scalatest.junit.JUnitRunner
import org.junit.runner.RunWith
import java.util.Date

/**                             
 * 
 * @author Oleg Ilyenko
 */
@RunWith(classOf[JUnitRunner])
class BooleanConverterTest extends WordSpec with ShouldMatchers {

    "Boolean converter" when {
        "used with implicit convertions" should {               
            import BooleanConverter._

            "convert string to false when it is empty" in {
                ("hello" || "") should be === true
                ("  " || "") should be === false
            }

            "convert Option of any type to false when it is None" in {
                (Some("String") || None) should be === true
                (None || None) should be === false
            }

            "convert any collection to false when it's empty" in {
                (List("Hello", "World", "!") || List()) should be === true
                (Set("Hello", "World", "!") || Set()) should be === true
                (Map("Hello" -> "World!") || Map()) should be === true
                (List() || List()) should be === false
                (Set() || Set()) should be === false
                (Map() || Map()) should be === false
            }
            
            "convert any other (unsupported) type to false when it is null" in {
                val nullUrl: URL = null
                val someUrl = new URL("http://hello.com")
                (someUrl || nullUrl) should be === true
                (nullUrl || nullUrl) should be === false
            }

            "secure supported types from null values" in {
                val nullString: String = null
                (nullString || "") should be === false
            }
        }
    }
    
}