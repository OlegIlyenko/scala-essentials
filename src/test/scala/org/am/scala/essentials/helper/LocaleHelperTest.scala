package org.am.scala.essentials.helper

import org.scalatest.WordSpec
import java.util.Locale
import org.scalatest.matchers.ShouldMatchers
import IoHelper._
import org.scalatest.junit.JUnitRunner
import org.junit.runner.RunWith

/**
 * 
 * @author Oleg Ilyenko
 */
@RunWith(classOf[JUnitRunner])
class LocaleHelperTest extends WordSpec with ShouldMatchers {

    "Locale helper" should {
        import LocaleHelper._                      

        val deDeAa = new Locale("de", "DE", "AA")
        val deDe = new Locale("de", "DE")
        val enUs = new Locale("en", "US")
        val jp = new Locale("jp")
        val available = List(deDeAa, deDe, enUs, jp)

        "find exactly same locale when it is available" in {
            findLocale(deDeAa, available) should be (Some(deDeAa))
            findLocale(deDe, available) should be (Some(deDe))
            findLocale(enUs, available) should be (Some(enUs))
            findLocale(jp, available) should be (Some(jp))
        }

        "find without variant when lang and country available" in {
            findLocale(new Locale("en", "US", "AA"), available) should be (Some(enUs))
        }

        "find without country and variant when lang available" in {
            findLocale(new Locale("jp", "BB", "AA"), available) should be (Some(jp))
            findLocale(new Locale("jp", "GB"), available) should be (Some(jp))
        }

        "return None if nothing matches" in {
            findLocale(new Locale("fr", "BB", "AA"), available) should be (None)
            findLocale(new Locale("fr", "BB"), available) should be (None)
            findLocale(new Locale("fr"), available) should be (None)
            findLocale(new Locale("en"), available) should be (None)
        }
    }

    "Localized file" should {
        "produce correct string like baseName_en_US_AA.properties" in {
            val fileName = LocalizedFile.toString("baseName", new Locale("en", "US", "AA"), "properties")
            fileName should be === "baseName_en_US_AA.properties"
        }

        "match filename with full localization information" in {
            "baseName_en_US_AA.properties" match {
                case LocalizedFile(baseName, locale, ext) =>
                    baseName should be === "baseName"
                    locale should be === Some(new Locale("en", "US", "AA"))
                    ext should be === "properties"
                case _ => fail("Does not match :(")
            }
        }

        "match filename with partal localization information" in {
            "baseName_en_US.properties" match {
                case LocalizedFile(baseName, locale, ext) =>
                    baseName should be === "baseName"
                    locale should be === Some(new Locale("en", "US"))
                    ext should be === "properties"
                case _ => fail("Does not match :(")
            }

            "baseName_en.properties" match {
                case LocalizedFile(baseName, locale, ext) =>
                    baseName should be === "baseName"
                    locale should be === Some(new Locale("en"))
                    ext should be === "properties"
                case _ => fail("Does not match :(")
            }
        }

        "match filename without localization information (locale should be None)" in {
            "baseName.properties" match {
                case LocalizedFile(baseName, locale, ext) =>
                    baseName should be === "baseName"
                    locale should be === None
                    ext should be === "properties"
                case _ => fail("Does not match :(")
            }
        }

        "match file" in {
            File("baseName.properties") match {
                case LocalizedFile(baseName, locale, ext) =>
                    baseName should be === "baseName"
                    locale should be === None
                    ext should be === "properties"
                case _ => fail("Does not match :(")
            }
        }

        "construct new file according to the available files and desired locale" in {
            LocalizedFile(testDir, "test", new Locale("de", "DE"), "properties") match {
                case Some(file) => file.getName should be ("test_de_DE.properties")
                case None => fail("file not found")
            }
        }

        "retutn default file when it exists" in {
             LocalizedFile(testDir, "test", new Locale("jp"), "properties") match {
                case Some(file) => file.getName should be ("test.properties")
                case None => fail("file not found")
            }
        }

        "return None when file does not exist" in {
             LocalizedFile(testDir, "test1", new Locale("en"), "properties") should be (None)
        }
    }

    def testDir = File(this.getClass.getResource("/org/am/scala/essentials/helper/test.properties").getFile).getParentFile
}