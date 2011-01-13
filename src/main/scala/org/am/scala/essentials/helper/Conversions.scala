package org.am.scala.essentials.helper

/**
 * 
 * @author Oleg Ilyenko
 */
object Conversions {
    implicit def anythingToOption[T](something: T) = if (something != null) Some(something) else None
    implicit def symbolToString(symbol: Symbol) = symbol.toString.substring(1)
}