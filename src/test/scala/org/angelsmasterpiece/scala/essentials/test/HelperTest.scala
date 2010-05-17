package org.angelsmasterpiece.scala.essentials.test

import org.junit.Test
import io.Source
import org.angelsmasterpiece.scala.essentials.helper.IoHelper._
import org.angelsmasterpiece.scala.essentials.helper.LocalizedFile


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