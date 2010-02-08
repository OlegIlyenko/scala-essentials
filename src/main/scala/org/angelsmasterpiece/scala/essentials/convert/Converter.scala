package org.angelsmasterpiece.scala.essentials.convert

import java.io.File

/**
 * Base class for all converters
 * 
 * @author Oleg Ilyenko
 */
abstract class Converter[From, To] (implicit fromManifest: Manifest[From], toManifest: Manifest[To]) {

    def supports(from: Class[_], to: Class[_])  = fromManifest.erasure.isAssignableFrom(from) && toManifest.erasure.isAssignableFrom(to)

    def convert(from: From): To
}

object DefaultConverter extends AggregatingConverter(List(
    new StringToString,
    new StringToInt,
    new StringToBoolean,
    new StringToDouble,
    new StringToFile
))

class AggregatingConverter (converters:List[Converter[_, _]]) {
    def convert[From, To](from: From) (implicit fromManifest: Manifest[From], toManifest: Manifest[To]): Option[To] =
        converters.find(_.supports(fromManifest.erasure, toManifest.erasure)) match {
            case Some(converter) => Some(converter.asInstanceOf[Converter[From, To]].convert(from))
            case None => None
        }
}

class StringToString extends Converter[String, String] {
    def convert(from: String) = from
}

class StringToInt extends Converter[String, Int] {
    def convert(from: String) = from.toInt
}

class StringToBoolean extends Converter[String, Boolean] {
    def convert(from: String) = from.toBoolean
}

class StringToDouble extends Converter[String, Double] {
    def convert(from: String) = from.toDouble
}

class StringToFile extends Converter[String, File] {
    def convert(from: String) = new File(from)
}