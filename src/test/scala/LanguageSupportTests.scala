//@
package xyz.hyperreal.bvm

import org.scalatest._
import prop.PropertyChecks

import scala.util.parsing.input.Position


class LanguageSupportTests extends FreeSpec with PropertyChecks with Matchers {

  val constants =
    Map(
      "write" -> {
        (_: VM, apos: Position, ps: List[Position], args: Any) =>
          val list =
            args match {
              case a: ArgList => a.array toList
              case a => List( a )
            }

          println( list map (a => display(deref(a))) mkString ", " )
      }
    )

 "hello world" in {
    val program =
      SourceAST( List(
        ApplyExpressionAST(
          null,
          VariableExpressionAST( null, "write", "write" ),
          null,
          List(
            (null, LiteralExpressionAST( "Hello world!" ))
          ),
          false
        )
      ))

    runCapture( program, constants, Map(), Map() ) shouldBe "Hello world!"
    //      """
    //        |(3, 4), 3, 4
    //        |(5, 6), 5, 6
    //      """.stripMargin.trim
  }

}