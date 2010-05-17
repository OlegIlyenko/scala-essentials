package org.angelsmasterpiece.scala.essentials.option

import collection._
import mutable.{ListBuffer}
import org.angelsmasterpiece.scala.essentials.convert.{AggregatingConverter, DefaultConverter}

/**
 *
 * @author Oleg Ilyenko
 */

trait CommandLine {

    val longPrefix = "--"
    val shortPrefix = "-"

    protected val converter = DefaultConverter

    def parse(source: Array[String]): Option[List[ValidationError]] = {
        val errors = new ListBuffer[ValidationError]

        parseArguments(source, errors)
        validateAbsentArguments(errors)
        falseAbsentBooleanArguments()

        if (errors.isEmpty) None else Some(errors.toList)
    }

    private def parseArguments(source: Array[String], errors: ListBuffer[ValidationError]) = {
        var currentArg: Option[Argument[_]] = None

        source.foreach { inputArgument =>
            currentArg = getArgumentFindFunction(inputArgument) match {
                case Some(findFunction) => findArgument(findFunction, inputArgument, currentArg, errors)
                case None => findArgumentParameter(inputArgument, currentArg, errors)
            }
        }
    }

    private def getArgumentFindFunction(argument: String) =
        if (argument startsWith longPrefix)
            Some((a: Argument[_]) => (longPrefix + a.longName) == argument)
        else if (argument startsWith shortPrefix)
            Some((a: Argument[_]) => (shortPrefix + a.shortName) == argument)
        else
            None

    private def findArgument(findFunction: Argument[_] => Boolean,
                             inputArgument: String,
                             currentArg: Option[Argument[_]],
                             errors: ListBuffer[ValidationError]) =
        arguments.find(findFunction) match {
            case None =>
                errors += ValidationError(None, "Undefined argument " + inputArgument)
                currentArg
            case Some(a) =>
                if (a.parameter)
                    Some(a)
                else {
                    a.asInstanceOf[Argument[Boolean]].value = Some(true)
                    None
                }
        }

    private def findArgumentParameter(inputArgument: String,
                                      currentArg: Option[Argument[_]], 
                                      errors: ListBuffer[ValidationError]) =
        currentArg match {
            case Some(a) =>
                setArgumentParameter(inputArgument, a, errors)
                None
            case None =>
                errors += ValidationError(None, "Strange parameter '" + inputArgument + "'. It does not belongs to any argument")
                currentArg
        }

    private  def setArgumentParameter(inputArgument: String, argument: Argument[_], errors: ListBuffer[ValidationError]) =
        argument.setValue(inputArgument, converter) match {
            case Some(err) => errors ++= err
            case None =>
        }

    private def validateAbsentArguments(errors: ListBuffer[ValidationError]) =
        arguments.filter(a => !a.present && !a.optional && !errors.exists(e => e.argument.isDefined && e.argument.get == a))
            .foreach(a => errors += ValidationError(Some(a), "Argument not set!"))

    private def falseAbsentBooleanArguments() =
        arguments.filter(a => !a.present && !a.parameter).foreach(_.asInstanceOf[Argument[Boolean]].value = Some(false))

    def parseOrExit(source: Array[String]): Unit = parse(source) match {
        case Some(errors) =>
            println(errors.map(_.toString).mkString("\n"))
            println
            println(usage)
            System.exit(1)
         case None =>
    }

    def usage: String = usageTitle + usageBody

    protected def usageTitle = "Usage:\n\tapplication.executable [ARGUMENTS]\n\nArguments:\n"

    protected def usageBody = arguments.map(getArgumentUsage).mkString("\n")

    protected def getArgumentUsage(a: Argument[_]) =
        " \t" + longPrefix + a.longName +
        (if (a.shortName != null) ", " + shortPrefix + a.shortName else "") +
        (if (a.parameter) " [" + (if (a.parameterName.isDefined) a.parameterName.get else a.valueType.getName) +  "]" else "") +
        (if(!a.optional) " (required)" else "") +
        "\n\t\t" +
        (if (a.description != null) a.description + "\n" else "")

    def apply(name: String): Option[_] = arguments.find(a => (a.alias.isDefined && a.alias.get == name) || a.longName == name) match {
        case Some(a) => a.value
        case None => None
    }

    def arguments: List[Argument[_]] = this.getClass.getMethods
        .filter(m => classOf[Argument[_]].isAssignableFrom(m.getReturnType))
        .map(m => m.invoke(this).asInstanceOf[Argument[_]])
        .sortBy(_.longName)
        .toList
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
            case Some(result) =>
                v = validate(result) match {
                    case Some(err) => errors ++= err; None
                    case None => Some(result)
                }
            case None => errors += ValidationError(Some(this), "Unable to convert parameter '" + arg + "' of type " + valueType.getName)
        }

        if (errors.isEmpty) None else Some(errors.toList)
    }

    private def validate(newValue: T) = validator match {
        case Some(validate) => validate(this, newValue) match {
            case Some(err) => Some(err)
            case None => None
        }
        case None => None
    }

    def apply(): T = value match {
        case Some(v) => v
        case None => throw new IllegalStateException("Value for the argument '" + longName + "' is not set!")
    }

    def present = value.isDefined
}

object FlagArgument {
    def apply(longName: String, shortName: String = null, description: String = null, alias: Option[String] = None) =
        Argument[Boolean](longName, shortName, description, alias = alias, parameter = false, optional = true)
}

case class ValidationError(val argument: Option[Argument[_]], val errorDescription: String) {
    override def toString = "Problem with argument" + (argument match {
        case Some(arg) => " '" + arg.longName + "'"
        case None => ""
    })  + ": " + errorDescription
}