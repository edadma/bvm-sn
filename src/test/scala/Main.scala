//@
package xyz.hyperreal.bvm

import scala.util.parsing.input.Position


object Main extends App {

  val program =
    SourceAST( List(
      ApplyExpressionAST(
        null,
        VariableExpressionAST( null, "write", "write" ),
        null,
        List(
          (null, SysvarExpressionAST( null, "args" ))
        ),
        false
      ),
    ))
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
  val arg = "asdf"

  run( program, constants, Map("args" -> ((vm: VM) => vm.args)), Map(), arg )

}