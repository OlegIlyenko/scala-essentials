package org.am.scala.essentials.test

import io.Source
import org.am.scala.essentials.helper.IoHelper._
import org.am.scala.essentials.helper.LocalizedFile
import org.junit.Test


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