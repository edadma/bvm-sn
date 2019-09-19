package xyz.hyperreal.funl2

import org.scalatest._
import prop.PropertyChecks
import xyz.hyperreal.bvm._

import scala.collection.immutable.TreeMap


class Tests extends FreeSpec with PropertyChecks with Matchers {

	"named capture groups" in {
		Pattern.compile( """^(?<a>[a-z]+)-(?<n>[0-9]+)$""" ).allMatches( """xyzzy-14""" ) shouldBe
			Stream(
				TreeMap(
					"0" -> (0, 8, new SubSequence("xyzzy-14")),
					"a" -> (0, 5, new SubSequence("xyzzy")),
					"n" -> (6, 8, new SubSequence("14"))))
	}

}