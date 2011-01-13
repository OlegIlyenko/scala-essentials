package org.am.scala.essentials.config

import java.util.Properties
import java.io.{FileInputStream, File}
import org.am.scala.essentials.option.CommandLine

/**
 * The source for the configuration
 * 
 * @author Oleg Ilyenko
 */
trait ConfigurationSource {
    def apply(name: String): PropertyValue[_]
}

sealed trait PropertyValue[T] {
    def isEmpty: Boolean
    def get: T
}

case object EmptyPropertyValue extends PropertyValue[Nothing] {
    def get = throw new IllegalStateException("No value")
    def isEmpty = true
}

case class RawPropertyValue(value: String) extends PropertyValue[String] {
    def get = value
    def isEmpty = false
}

case class RealPropertyValue[T](value: T) extends PropertyValue[T] {
    def get = value
    def isEmpty = false
}

// Implementations

class SystemConfigurationSource extends ConfigurationSource {
    def apply(name: String) = if (System.getProperty(name) != null) RawPropertyValue(System.getProperty(name)) else EmptyPropertyValue
}

case class MapConfigurationSource(val properties: Map[String, String]) extends ConfigurationSource {
    def apply(name: String) = properties.get(name) match {
        case Some(v) => RawPropertyValue(v)
        case None => EmptyPropertyValue
    }
}

case class PropertiesConfigurationSource(val properties: Properties) extends ConfigurationSource {
    def apply(name: String) = if (properties.getProperty(name) != null) RawPropertyValue(properties.getProperty(name)) else EmptyPropertyValue
}

object PropertiesConfigurationSource {
    def apply(fileName: String):PropertiesConfigurationSource = apply(new File(fileName))
    def apply(file: File):PropertiesConfigurationSource = apply(loadFile(file))

    def loadFile(file: File) = {
        val props = new Properties()
        props load (new FileInputStream(file))
        props
    }
}

case class CommandLineConfigurationSource(val cl: CommandLine) extends ConfigurationSource {
    def apply(name: String) = cl(name) match {
        case Some(v) => RealPropertyValue(v)
        case None => EmptyPropertyValue
    }
}

case class AggregatingConfigurationSource(val sources: List[ConfigurationSource]) extends ConfigurationSource {
    def apply(name: String): PropertyValue[_] = sources.find(!_(name).isEmpty) match {
        case Some(cs) => cs(name)
        case None => EmptyPropertyValue
    }
}

object AggregatingConfigurationSource {
    def apply(acss: ConfigurationSource*):AggregatingConfigurationSource = new AggregatingConfigurationSource(acss.toList)
}