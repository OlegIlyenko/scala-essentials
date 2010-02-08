package org.angelsmasterpiece.scala.essentials.config

import java.lang.String
import reflect.Manifest
import org.angelsmasterpiece.scala.essentials.convert._

/**
 *
 * @author Oleg Ilyenko
 */
trait Configuration {

    protected val converter = DefaultConverter

    implicit private var configSource: ConfigurationSource = new SystemConfigurationSource

    case class Property[T](val name: String, val description: String = "", val default: Option[T] = None)
                          (implicit source: ConfigurationSource = new SystemConfigurationSource, m: Manifest[T]) {
        def apply(): T = source(name) match {
            case RawPropertyValue(value) => try {
                converter.convert[String, T](value) match {
                    case Some(result) => result
                    case None => throw new IllegalArgumentException("Property '" + name + "' cannot be coverted. Converter for the class '" + m.erasure + "' cannot befound!")
                }
            } catch {
                case e:Exception => throw new IllegalArgumentException("Property '" + name + "' cannot be coverted. Cause: " + e.getMessage, e)
            }
            case RealPropertyValue(realValue) => realValue.asInstanceOf[T]
            case  EmptyPropertyValue => default match {
                case Some(dv) => dv
                case None => throw new IllegalArgumentException("Property '" + name + "' cannot be found!")
            }
        }
    }

    def validate(): Unit = this.getClass.getMethods.filter(m => classOf[Property[_]].isAssignableFrom(m.getReturnType) || classOf[Configuration].isAssignableFrom(m.getReturnType)).map { m =>
        if (classOf[Property[_]].isAssignableFrom(m.getReturnType)) m.invoke(this).asInstanceOf[Property[_]]()
        else m.invoke(this).asInstanceOf[Configuration].validate()
    }

    def toString(prefix:String):String = this.getClass.getMethods
        .filter(m => classOf[Property[_]].isAssignableFrom(m.getReturnType) || classOf[Configuration].isAssignableFrom(m.getReturnType))
        .map { m =>
            if (classOf[Property[_]].isAssignableFrom(m.getReturnType)) {
                val prop:Property[_] = m.invoke(this).asInstanceOf[Property[_]]
                prefix + m.getName  +
                    (if (prop.default != null) " (default: " + prop.default + ")" else "") +
                    " - " + prop.name + "" + "\n      " +
                    (if (prop.description != "") prop.description + "\n" else "")
            } else m.invoke(this).asInstanceOf[Configuration].toString(m.getName + ".")
        }.mkString("\n")

    override def toString = toString("")
}
