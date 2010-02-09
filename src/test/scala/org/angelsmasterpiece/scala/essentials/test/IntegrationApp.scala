package org.angelsmasterpiece.scala.essentials.test

import org.angelsmasterpiece.scala.essentials.option.{Argument, FlagArgument, CommandLine}
import java.io.File
import org.angelsmasterpiece.scala.essentials.config._
import org.angelsmasterpiece.scala.essentials.helper.Conversions._

/**
 *
 * @author Oleg Ilyenko
 */
object IntegrationApp {

    def main(args: Array[String]) {

        class MyCommandLine extends CommandLine {
            val argument1 = Argument[String]("argument1", optional = true)
            val argument2 = Argument[Double]("argument2", "arg2", description = "Double argument")
            val help = FlagArgument("help", "h", description = "Show this help")

            val flag = FlagArgument("flag-arg", "fl", "My super flag")
        }
        
        val cl = new MyCommandLine
        cl parseOrExit args

        cl.argument1.value match {
            case Some(v) =>
                println("Pattern -> Some")
                println("Argument 1> " + cl.argument1())
            case None => println("Pattern -> None")
        }

        println("Argument 2> " + cl.argument2())
        println("Argument 3> " + cl.argument2())
        println("Argument 4> " + cl.flag())

        case class CLConfig(s: ConfigurationSource) extends Configuration {
            implicit val implicitConfigurationSource = s

            val welcomeString = Property[Boolean]("welcome", default = false)
            val argument3 = Property[Double]("argument3")
            val mapprop = Property[String]("onlyInMap")

            validate()
        }

        case class MyConfig(s: ConfigurationSource) extends Configuration {

            // You can provice source like this.
            // By default SystemConfigurationSource would be used.
            implicit val implicitConfigurationSource = s

            val appName = Property[String]("common.appName", "Application name", "My app")
            val version = Property[Double]("common.version", "Application version", default = 2.1)
            val file = Property[File]("common.file", description = "Additional configuration file")

            val database = new Configuration {
                val driver = Property[String]("database.driver", default = "com.mysql.jdbc.Driver")
                val url = Property[String]("database.url", "JDBC Database URL")
            }

            // It's optional step, but you can use it for validation during instantiation.
            // You can also validate later manually by invoking validate(method on configuration instance)
            validate()
        }

        val config = MyConfig(PropertiesConfigurationSource("config.properties"))
        println(config.appName())
        println(config.database.driver())
        println(config.database.url())

        val clCnf = CLConfig(AggregatingConfigurationSource(CommandLineConfigurationSource(cl), MapConfigurationSource(Map(
            "onlyInMap" -> "Hi"
        ))))

        println("cl cnf 1> " + clCnf.welcomeString())
        println("cl cnf 2> " + clCnf.argument3())
        println("cl cnf 3> " + clCnf.mapprop())
    }
}