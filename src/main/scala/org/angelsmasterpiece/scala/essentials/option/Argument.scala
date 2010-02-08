package org.angelsmasterpiece.scala.essentials.option

import collection._
import mutable.{ListBuffer}
import org.angelsmasterpiece.scala.essentials.convert.{AggregatingConverter, DefaultConverter}

/**
 *
 * @author Oleg Ilyenko
 */

trait CommandLine {

    protected val converter = DefaultConverter

    def parse(source: Array[String]): Option[List[ValidationError]] = {
        val errors = new ListBuffer[ValidationError]
        val definedArgs = arguments

        var currentArg: Option[Argument[_]] = None

        source.foreach { arg =>
            val findFn = if (arg startsWith CommandLine.LongPrefix)
                Some((a: Argument[_]) => (CommandLine.LongPrefix + a.longName) == arg)
            else if (arg startsWith CommandLine.ShortPrefix)
                Some((a: Argument[_]) => (CommandLine.ShortPrefix + a.shortName) == arg)
            else
                None

            findFn match {
                case Some(fn) => definedArgs.find(fn) match {
                    case None => errors += ValidationError(None, "Undefined argument " + arg)
                    case Some(a) => if (a.parameter) currentArg = Some(a) else a.asInstanceOf[Argument[Boolean]].value = Some(true)
                }
                case None => currentArg match {
                    case Some(a) =>
                        a.setValue(arg, converter) match {
                            case Some(err) => errors ++= err
                            case None =>
                        }
                        currentArg = None
                    case None => errors += ValidationError(None, "Strange parameter '" + arg + "'. It does not belongs to any argument")
                }
            }
        }

        definedArgs.filter(a => !a.present && !a.optional && !errors.exists(e => e.argument.isDefined && e.argument.get == a))
            .foreach(a => errors += ValidationError(Some(a), "Argument not set!"))
        
        definedArgs.filter(a => !a.present && !a.parameter).foreach(_.asInstanceOf[Argument[Boolean]].value = Some(false))

        if (errors.isEmpty) None else Some(errors.toList)
    }

    def parseOrExit(source: Array[String]): Unit = (parse(source): @unchecked) match {
        case Some(errors) =>
            println(errors.map(_.toString).mkString("\n"))
            println
            println(usage)
            System.exit(1)
         case None =>
    }

    def usage: String = "Usage:\n application.executable [ARGUMENTS]\n\nArguments:\n" + arguments
        .map(a =>
                " \t" + CommandLine.LongPrefix + a.longName +
                (if (a.shortName != null) ", " + CommandLine.ShortPrefix + a.shortName else "") +
                (if (a.parameter) " [" + (if (a.parameterName.isDefined) a.parameterName.get else a.valueType.getName) +  "]" else "") +
                (if(!a.optional) " (required)" else "") +
                "\n\t\t" +
                (if (a.description != null) a.description + "\n" else ""))
        .mkString("\n")

    def apply(name: String): Option[_] = arguments.find(a => (a.alias.isDefined && a.alias.get == name) || a.longName == name) match {
        case Some(a) => a.value
        case None => None
    }

    def arguments: List[Argument[_]] = this.getClass.getMethods
        .filter(m => classOf[Argument[_]].isAssignableFrom(m.getReturnType))
        .map(m => m.invoke(this).asInstanceOf[Argument[_]]).sortBy(_.longName).toList

}

object CommandLine {
    val LongPrefix = "--"
    val ShortPrefix = "-"
}

case class Argument[T](
    val longName: String,
    val shortName: String = null,
    val description: String = null,
    val optional:Boolean = false,
    val parameter: Boolean = true,
    val parameterName: Option[String] = None,
    val alias: Option[String] = None,
    val validator: Option[(Argument[T], T) => Option[immutable.List[ValidationError]]] = None
) (implicit manifest: Manifest[T]) {

    private var v: Option[T] = None

    def value = v
    def value_=(value: Option[T]) = v = value

    def valueType = manifest.erasure

    def setValue(arg: String, converter: AggregatingConverter): Option[List[ValidationError]] = {
        val errors = new ListBuffer[ValidationError]
        
        converter.convert[String , T](arg) match {
            case Some(result) => v = validator match {
                case Some(validate) => validate(this, result) match {
                    case Some(err) =>
                        errors ++= err
                        None    
                    case None => Some(result)
                }
                case None => Some(result)
            }
            case None => errors += ValidationError(Some(this), "Unable to convert parameter '" + arg + "' of type " + valueType.getName)
        }

        if (errors.isEmpty) None else Some(errors.toList)
    }

    def apply(): T = value match {
        case Some(v) => v
        case None => throw new IllegalStateException("Value for the argument " + CommandLine.LongPrefix + longName + " is not set!")
    }

    def present = value.isDefined
}

object FlagArgument {
    def apply(longName: String, shortName: String = null, description: String = null, alias: Option[String] = None) =
        Argument[Boolean](longName, shortName, description, alias = alias, parameter = false, optional = true)
}

case class ValidationError(val argument: Option[Argument[_]], val errorDescription: String) {
    override def toString = "Problem with argument" + (argument match {
        case Some(arg) => " " + CommandLine.LongPrefix + arg.longName
        case None => ""
    })  + ": " + errorDescription
}