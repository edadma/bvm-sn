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
          (null, DotExpressionAST( null, ApplyExpressionAST( null, SysvarExpressionAST(null, "args"), null, List((null, LiteralExpressionAST(0))), false ), null, 'v ))
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

  run( program, constants, Map("args" -> ((vm: VM) => vm.args)), Map(), new C( 123 ) )

}

class C( var v: Int )