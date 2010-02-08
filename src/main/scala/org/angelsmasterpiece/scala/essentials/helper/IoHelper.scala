package org.angelsmasterpiece.scala.essentials.helper

import io.Source
import java.io._
import collection.mutable.ListBuffer

/**
 *
 * @author Oleg Ilyenko
 */
object IoHelper {

    implicit def fileToRichFile(f: File) = new RichFile(f)

    class RichFile(f: File) {

        def text: Option[String] = if (f.exists) Some(closeable(new FileInputStream(f)) (Source.fromInputStream(_).getLines().mkString("\n"))) else None

        def lines: Option[List[String]] = if (f.exists) Some(closeable(new BufferedReader(new FileReader(f))) { reader =>
            val list = new ListBuffer[String]

            var line: String = reader.readLine()
            while (line != null) {
                list += line
                line = reader.readLine()
            }

            list.toList
        }) else None

        def write(text: String): Unit = closeable (new PrintWriter(f)) (_.write(text))
    }

    object File {
        def apply (fileName: String) = new File(fileName)
        def apply (parent: File, fileName: String) = new File(parent, fileName)
    }

    // Closeasble

    def closeable[Res0 <: Closeable, Result](resource0: Res0)(block: Res0 => Result): Result = try {
        block(resource0)
    } finally {
        if (resource0 != null) {
            resource0.close()
        }
    }

    def closeable[Res0 <: Closeable, Res1 <: Closeable, Result](resource0: Res0, resource1: Res1)(block: (Res0, Res1) => Result): Result = try {
        try {
            block(resource0, resource1)
        } finally {
            if (resource1 != null) {
                resource1.close()
            }
        }
    } finally {
        if (resource0 != null) {
            resource0.close()
        }
    }

    def closeable[Res0 <: Closeable, Res1 <: Closeable, Res2 <: Closeable, Result](resource0: Res0, resource1: Res1, resource2: Res2)(block: (Res0, Res1, Res2) => Result): Result = try {
        try {
            try {
                block(resource0, resource1, resource2)
            } finally {
                if (resource2 != null) {
                    resource2.close()
                }
            }
        } finally {
            if (resource1 != null) {
                resource1.close()
            }
        }
    } finally {
        if (resource0 != null) {
            resource0.close()
        }
    }
}