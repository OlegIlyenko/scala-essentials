package org.am.scala.essentials.helper

import java.util.Locale
import java.io.File

import BooleanConverter._

/**
 * 
 * @author Oleg Ilyenko
 */
object LocaleHelper {
    def findLocale(desired: Locale, available: List[Locale]) =
        available.find(l => eq(desired.getLanguage, l.getLanguage) && eq(desired.getCountry, l.getCountry) && eq(desired.getVariant, l.getVariant)) match {
            case Some(locale) => Some(locale)
            case None => available.find(l => eq(desired.getLanguage, l.getLanguage) && eq(desired.getCountry, l.getCountry) && !l.getVariant) match {
                case Some(locale) => Some(locale)
                case None => available.find(l => eq(desired.getLanguage, l.getLanguage) && !l.getCountry && !l.getVariant) match {
                    case Some(locale) => Some(locale)
                    case None => None
                }
            }
        }

    private def eq(localePart1: String, localePart2: String) = localePart1 && localePart2 && localePart1 == localePart2
}

object LocalizedFile {
    def apply(parent: File, baseName: String, locale: Locale, extension: String): Option[File] = {
        val available = parent.listFiles.toList.flatMap(f => f match {
            case LocalizedFile(`baseName`, locale, `extension`) => locale
            case _ => None
        })

        LocaleHelper.findLocale(locale, available) match {
            case Some(foundLocale) => Some(new File(parent, toString(baseName, foundLocale, extension)))
            case None =>
                val file = new File(parent, baseName + "." + extension)
                if (file.exists)
                    Some(file)
                else
                    None
        }
    }

    def unapply(fileName: String): Option[(String, Option[Locale], String)] = {
        val idx = fileName.lastIndexOf('.')
        if (idx != -1) {
            val (baseName, locale) = fileName.substring(0, idx).split('_').toList.reverse match {
                case variant :: country :: lang :: rest if variant.length == 2 && country.length == 2 && lang.length == 2 =>
                    (rest.mkString("_"), Some(new Locale(lang, country, variant)))
                case country :: lang :: rest if country.length == 2 && lang.length == 2 =>
                    (rest.mkString("_"), Some(new Locale(lang, country)))
                case lang :: rest if lang.length == 2 =>
                    (rest.mkString("_"), Some(new Locale(lang)))
                case rest => (rest.mkString("_"), None)
            }

            Some((baseName, locale, fileName.substring(idx + 1)))
        } else None
    }

    def unapply(file: File): Option[(String, Option[Locale], String)] = if (!file.isDirectory) unapply(file.getName) else None

    def toString(baseName: String, locale: Locale, extension: String) = baseName + "_" + locale + "." + extension
}