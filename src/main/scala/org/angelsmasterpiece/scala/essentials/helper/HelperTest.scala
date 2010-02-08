package org.angelsmasterpiece.scala.essentials.helper

import org.junit.Test
import io.Source
import IoHelper._
import util.matching.Regex


/**
 * 
 * @author Oleg Ilyenko
 */
class HelperTest {

    @Test
    def testCloseable {

        val Link = """.*<a href="(.*)".*""".r

//        println (File("d:/Temp/links-go.html").text)

        println (File("d:/Temp/links-go.html").lines.map {
            case Link(link) => Some(link)
            case _ => None
        }.filter(_.isDefined).map(_.get.replaceAll("Hikaru", "Oleg")).mkString("\n"))

    }



}