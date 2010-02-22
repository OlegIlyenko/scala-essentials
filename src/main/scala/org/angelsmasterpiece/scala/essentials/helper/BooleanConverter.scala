package org.angelsmasterpiece.scala.essentials.helper

/**
 * 
 * @author Oleg Ilyenko
 */
object BooleanConverter {
    implicit def stringToBoolean(s: String) = s != null && s.trim.length != 0
    implicit def optionToBoolean(o: Option[_]) = o != null && o.isDefined
    implicit def optionToBoolean(t: Traversable[_]) = t != null && t.size != 0
    implicit def anyToBoolean(any: Any) = any != null
}