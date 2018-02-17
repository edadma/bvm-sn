//@
package xyz.hyperreal.bvm

import scala.util.parsing.input.Position


object Main extends App {

  val program =
    SourceAST( List(
      ValAST(
        VariableStructureAST( null, "a", "a" ),
        null,
        LiteralExpressionAST( "Hello world!" )
      ),
      ApplyExpressionAST(
        null,
        VariableExpressionAST( null, "write", "write" ),
        null,
        List(
          (null, VariableExpressionAST( null, "a", "a" ))
        ),
        false
      ),
      SetValueExpressionAST(
        null,
        "a",
        "a",
        LiteralExpressionAST( "bye bye" )
      ),
      ApplyExpressionAST(
        null,
        VariableExpressionAST( null, "write", "write" ),
        null,
        List(
          (null, VariableExpressionAST( null, "a", "a" ))
        ),
        false
      )
      //, AssignmentExpressionAST( List((null, VariableExpressionAST(null, "a", "a"))), '=, null, List((null, LiteralExpressionAST( "bye bye again" ))) )
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

  run( program, constants, Map(), Map() )

}