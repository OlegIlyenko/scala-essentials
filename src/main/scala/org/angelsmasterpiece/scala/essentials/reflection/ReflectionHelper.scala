package org.angelsmasterpiece.scala.essentials.reflection

import collection.mutable.ListBuffer
import java.lang.reflect.Method

/**
 * 
 * @author Oleg Ilyenko
 */
object ReflectionHelper {
    implicit def classToReflectionWrapper(cl: Class[_]) = new ReflectionWrapper(cl)
    implicit def objectToReflectionObjectWrapper(obj: Object) = new ReflectionObjectWrapper(obj)
}

class ReflectionWrapper(clazz: Class[_]) {
    import ReflectionHelper._

    /**
     * @returns all class/superclass/implemented trait names
     */
    def allClassNames: List[String] = allClasses.map(_.getSimpleName)

    def allClasses: List[Class[_]] = {
        val buffer = new ListBuffer[Class[_]]
        def hierarchyNames(c: Class[_]): List[Class[_]] =
            c :: (if (c.getSuperclass != null) hierarchyNames(c.getSuperclass) else Nil)

        buffer ++= hierarchyNames(clazz)
        buffer ++= clazz.getInterfaces.toList

        buffer.toList
    }

    /**
     * @returns method with provided name that satisfies provided conditions
     */
    def getMatchingMethod(name: String, returnType: Class[_], args: Class[_]*) = clazz.getMethods.find { method =>
        method.getName == name &&
        method.getReturnType.allClasses.contains(returnType) &&
        method.getParameterTypes.length == args.length &&
        !method.getParameterTypes.zipWithIndex.exists {case (cl, idx) => cl != args(idx)}
    }

    /**
     * @returns all methods that satisfy provided conditions
     */
    def getMatchingMethods(returnType: Class[_], args: Class[_]*) = clazz.getMethods.filter { method =>
        method.getReturnType.allClasses.contains(returnType) &&
        method.getParameterTypes.length == args.length &&
        !method.getParameterTypes.zipWithIndex.exists {case (cl, idx) => cl != args(idx)}
    } toList

    def getAnnotatedMethods(returnType: Class[_], args: Class[_]*) = clazz.getMethods.filter { method =>
        method.getReturnType.allClasses.contains(returnType) &&
        method.getParameterTypes.length == args.length &&
        !method.getParameterTypes.zipWithIndex.exists {case (cl, idx) => cl != args(idx)}
    } toList
}

class ReflectionObjectWrapper(obj: Object) {

    /**
     * @returns Some(value) of the <code>val</code> or None if such val not found
     */
    def getValValue[T](name: String)(implicit m: Manifest[T]): Option[T] =
        obj.getClass.getMethods.find(_.getName == name) match {
            case Some(method) =>
                if (m.erasure.isAssignableFrom(method.getReturnType) && method.getParameterTypes.length == 0)
                    Some(method.invoke(obj).asInstanceOf[T])
                else
                    None
            case None => None
        }
}