package xyz.hyperreal

import java.io.ByteArrayOutputStream

import scala.util.parsing.input.Position

import xyz.hyperreal.bvm.Pattern._
import xyz.hyperreal.lia.Math


package object bvm {

	type NativeFunction = (_, _, _, _) => _
	type cset = Char => Boolean

	val DIGIT_CLASS = (_: Char).isDigit
	val HEXDIGIT_CLASS = new CSet( DIGIT_CLASS, 'a' to 'f', 'A' to 'F' )
	val LETTER_CLASS = (_: Char).isLetter
	val ALPHANUM_CLASS = new CSet( LETTER_CLASS, DIGIT_CLASS )
	val WORD_CLASS = new CSet( ALPHANUM_CLASS, (_: Char) == '_' )
	val NONWORD_CLASS = new CSet( unionOf(ALPHANUM_CLASS, (_: Char) == '_') ).complement
	val WHITESPACE_CLASS = " \t\r\u000B\n".toSet
	val HORIZONTAL_WHITESPACE_CLASS = new CSet( '\u2000' to '\u200a', "\t\u00A0\u1680\u180e\u202f\u205f\u3000" )
	val VERTICAL_WHITESPACE_CLASS = "\n\u000B\f\r\u0085\u2028\u2029".toSet
	val TERMINATOR_CLASS = "\u000A\u000B\u000C\u000D\u0085\u2028\u2029".toSet

	val LineBreakPattern = Pattern.compiledSubpattern( FlagConditionalPattern(UNIX_LINES, StringPattern(LiteralExpressionAST("\n")), AlternationPattern(List(StringPattern(LiteralExpressionAST("\r\n")), ClassPattern(TERMINATOR_CLASS)))) )
	val BeginningOfLinePattern = Pattern.compiledSubpattern( FlagConditionalPattern(MULTILINE, AlternationPattern(List(FlagConditionalPattern(UNIX_LINES, LookbehindClassPattern(_ == '\n'), LookbehindClassPattern(TERMINATOR_CLASS)), BeginningOfInputPattern)), BeginningOfInputPattern) )
	val EndOfLinePattern = Pattern.compiledSubpattern( FlagConditionalPattern(MULTILINE, LookaheadPattern(AlternationPattern(List(LineBreakPattern, EndOfInputPattern))), EndOfInputPattern) )
	val EndOfInputBeforeFinalTerminatorPattern = Pattern.compiledSubpattern( LookaheadPattern(ConcatenationPattern(List(OptionalPattern(LineBreakPattern), EndOfInputPattern))) )
	val NonEmptyInputPattern = Pattern.compiledSubpattern( NegationPattern(ConcatenationPattern(List(BeginningOfInputPattern, EndOfInputPattern))) )
	val WordBoundaryPattern = Pattern.compiledSubpattern( AlternationPattern(List(ConcatenationPattern(List(LookbehindClassPattern(NONWORD_CLASS), LookaheadClassPattern(WORD_CLASS), NonEmptyInputPattern)), ConcatenationPattern(List(LookbehindClassPattern(WORD_CLASS), LookaheadClassPattern(NONWORD_CLASS), NonEmptyInputPattern)))) )
	val NonWordBoundaryPattern = Pattern.compiledSubpattern( NegationPattern(WordBoundaryPattern) )

	val HEXOCTET = Pattern.compiledSubpattern( ConcatenationPattern(List(ClassPattern(HEXDIGIT_CLASS))) )
	val UUID =
		Pattern.compiledSubpattern( ConcatenationPattern(List(
			hexOctets(4),
			LiteralPattern("-"),
			hexOctets(2),
			LiteralPattern("-"),
			hexOctets(2),
			LiteralPattern("-"),
			hexOctets(2),
			LiteralPattern("-"),
			hexOctets(6)
		)) )
	val INTEGER = Pattern.compiledSubpattern( OneOrMorePattern(ClassPattern(DIGIT_CLASS)) )
	val HEXINTEGER = Pattern.compiledSubpattern( OneOrMorePattern(ClassPattern(HEXDIGIT_CLASS)) )

	def hexOctets( octets: Int ) = RepeatPattern( HEXOCTET, octets, null, Some(octets) )

	def deref( a: Any ) =
		a match {
			case a: Assignable => a.value
			case _ => a
		}

	def argsderef( a: Any ) =
		a match {
			case a: ArgList => ArgList( a.array map deref: _* )
			case _ => deref( a )
		}

	def problem( pos: Position, error: String ) =
		if (pos eq null)
			sys.error( error )
		else
			sys.error( s"${pos.line}: $error\n${pos.longString}" )

	def run( ast: AST, constants: Map[String, Any], sysvars: Map[String, VM => Any],
					 macros: Map[String, List[AST] => AST], args: Any* ) = {
		val code = new Compiler( constants, sysvars, macros, comments = true ).compile( ast )
		val vm = new VM( code, Array(), false, false, args )

		vm.execute
	}

	def runCapture( ast: AST, constants: Map[String, Any], sysvars: Map[String, VM => Any],
									macros: Map[String, List[AST] => AST], args: Any* ): String = {
		val outCapture = new ByteArrayOutputStream

		Console.withOut(outCapture) {run( ast, constants, sysvars, macros )}
		outCapture.toString.trim
	}

	def displayQuoted( a: Any ): String =
		a match {
			case s: String =>
				var t = s

				for ((k, v) <- List( "\\" -> "\\\\", "\"" -> "\\\"", "\t" -> "\\t", "\b" -> "\\b", "\f" -> "\\f", "\n" -> "\\n", "\r" -> "\\r", "\b" -> "\\b" ))
					t = t.replaceAllLiterally( k, v )

				s""""$t""""
			case _ => display( a )
		}

	def display( a: Any ): String =
		a match {
			case a: Array[_] => a map display mkString ("Array(", ", ", ")")
			case l: List[_] => l map displayQuoted mkString ("[", ", ", "]")
			case s: LazyList[_] =>
				val howMany = 100
				val bunch = s take (howMany + 1)

				if (s isDefinedAt (howMany + 1))
					bunch take howMany map display mkString( "[", ", ", ", ...]" )
				else
					display( bunch toList )
			case s: collection.Set[_] if s isEmpty => "void"
			case s: collection.Set[_] => s.map( display ).mkString( "{", ", ", "}" )
			case m: collection.Map[_, _] if m isEmpty => "{}"
			case m: collection.Map[_, _] => m.toList.map( {case (k, v) => displayQuoted(k) + ": " + displayQuoted(v)} ).mkString( "{", ", ", "}" )
			case t: Vector[_] => t.map( display ).mkString( "<", ", ", ">" )
			case t: Tuple => t.map( display ).mkString( "(", ", ", ")" )
			case p: Product if p.productArity > 0 && !p.productPrefix.startsWith( "Tuple" ) =>
				p.productPrefix + '(' + p.productIterator.map( display ).mkString( ", " ) + ')'
			case p: Product if p.productArity == 0 => p.productPrefix
			//			case Some( a ) => "Some(" + display(a) + ")"
			case _ => String.valueOf( a )
		}

	val NUMERIC =
		new Numeric[Any] {
			def parseString( str: String ): Option[Any] = sys.error( "shouldn't call Numeric.parseString()" )

			def compare( x: Any, y: Any ): Int = naturalCompare( x, y )

			def fromInt( x: Int ) = Integer.valueOf( x )

			def minus( x: Any, y: Any ) = Math( Symbol("-"), x, y )

			def negate( x: Any ) = Math( Symbol("-"), x )

			def plus( x: Any, y: Any ) = Math( Symbol("+"), x, y )

			def times( x: Any, y: Any ) = Math( Symbol("*"), x, y )

			def toDouble( x: Any ) = x.asInstanceOf[Number].doubleValue

			def toFloat( x: Any ) = x.asInstanceOf[Number].floatValue

			def toInt( x: Any ) = x.asInstanceOf[Number].intValue

			def toLong( x: Any ) = x.asInstanceOf[Number].longValue
		}

	val ORDERING =
		new Ordering[Any]
		{
			def compare( x: Any, y: Any ): Int = naturalCompare( x, y )
		}

	def naturalCompare( x: Any, y: Any ): Int =
		(x, y) match
		{
			case (a: Number, b: Number) =>
				if (Math( Symbol("<"), a, b ).asInstanceOf[Boolean])
					-1
				else if (Math( Symbol(">"), a, b ).asInstanceOf[Boolean])
					1
				else
					0
			case (a: String, b: String) => a compare b
			case (a: Seq[Any], b: Seq[Any]) => lexicographicalCompare( a, b )
			case (a: Product, b: Product) if a.productPrefix == b.productPrefix => lexicographicalCompare( a.productIterator.toSeq, b.productIterator.toSeq )
			case _ => sys.error( s"non-comparable: $x, $y" )
		}

	def lexicographicalCompare( a: Seq[Any], b: Seq[Any] ): Int =
	{
		for ((u, v) <- a zip b)
			if (u != v)
				return naturalCompare( u, v )

		val (alen, blen) = (a.length, b.length)

		if (alen < blen)
			-1
		else if (alen > blen)
			1
		else
			0
	}

}