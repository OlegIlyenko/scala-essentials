package org.angelsmasterpiece.scala.essentials.config

import java.lang.String
import reflect.Manifest
import org.angelsmasterpiece.scala.essentials.convert._
import java.lang.reflect.Method

/**
 *
 * @author Oleg Ilyenko
 */
trait Configuration {
    protected val converter = DefaultConverter

    implicit private var configSource: ConfigurationSource = new SystemConfigurationSource

    case class Property[T](
        val name: String,
        val description: String = "",
        val default: Option[T] = None
    ) (implicit source: ConfigurationSource = new SystemConfigurationSource, m: Manifest[T]) {

        def apply(): T = source(name) match {
            case RawPropertyValue(value) => getRawProperty(value)
            case RealPropertyValue(realValue) => realValue.asInstanceOf[T]
            case EmptyPropertyValue => default match {
                case Some(dv) => dv
                case None => throw new IllegalArgumentException("Property '" + name + "' cannot be found!")
            }
        }

        def is: T = apply()

        def set_?(): Boolean = source(name) match {
            case EmptyPropertyValue => false
            case _ => true
        }

        def default_?(): Boolean = source(name) match {
            case EmptyPropertyValue => default match {
                case Some(dv) => true
                case None => false
            }
            case _ => false
        }

        private def getRawProperty(value: String) = try {
            convertRawProperty(value)
        } catch {
            case e: Exception => throw new IllegalArgumentException("Property '" + name + "' cannot be coverted. Cause: " + e.getMessage, e)
        }

        private def convertRawProperty(value: String) = converter.convert[String, T](value) match {
            case Some(result) => result
            case None => throw new IllegalArgumentException("Converter for the class '" + m.erasure + "' cannot befound!")
        }
    }

    def validate(): Unit = supportedChildren.map { m =>
        if (isProperty(m.getReturnType))
            m.invoke(this).asInstanceOf[Property[_]]()
        else
            m.invoke(this).asInstanceOf[Configuration].validate()
    }

    private def supportedChildren = this.getClass.getMethods.filter(isSupportedChild(_))

    private def isSupportedChild(method: Method) = isProperty(method.getReturnType) || isConfiguration(method.getReturnType)

    private def isProperty(clazz: Class[_]) = classOf[Property[_]].isAssignableFrom(clazz)

    private def isConfiguration(clazz: Class[_]) = classOf[Configuration].isAssignableFrom(clazz)

    def toString(prefix: String): String = supportedChildren.map { m =>
        if (isProperty(m.getReturnType))
            renderMethod(m, prefix)
        else
            m.invoke(this).asInstanceOf[Configuration].toString(m.getName + ".")
    }.mkString("\n")

    private def renderMethod(method: Method, prefix: String) = {
        val prop: Property[_] = method.invoke(this).asInstanceOf[Property[_]]
        prefix + method.getName + renderDefaultValue(prop) + renderProperty(prop) + renderDescription(prop)
    }

    private def renderDefaultValue(prop: Property[_]) =
        if (prop.default != null)
            " (default: " + prop.default + ")"
        else
            ""

    private def renderProperty(prop: Property[_]) = " - " + prop.name + "\n      "

    private def renderDescription(prop: Property[_]) =
        if (prop.description != "")
            prop.description + "\n"
        else
            ""

    override def toString = toString(prefix = "")
}