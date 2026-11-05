package tests.pc

import scala.meta.internal.pc.ScalaPresentationCompiler

import tests.BasePCSuite

/** Navigation and SemanticDB must not give one overload another body identity. */
class SemanticdbOverloadIdentitySuite extends BasePCSuite {
  test("orphan-overloads-serialize-the-current-compiler-selected-peer") {
    val global =
      presentationCompiler.asInstanceOf[ScalaPresentationCompiler].newCompiler()
    try {
      import global._
      import semanticdbOps._
      val owner = EmptyPackageClass.newClassSymbol(TypeName("DiagnosticOwner"))
      val declarations = newScope
      owner.setInfo(
        ClassInfoType(List(definitions.ObjectTpe), declarations, owner)
      )
      val first = owner.newMethodSymbol(TermName("selected"))
      first.setInfo(NullaryMethodType(definitions.IntTpe))
      val second = owner.newMethodSymbol(TermName("selected"))
      second.setInfo(NullaryMethodType(definitions.StringTpe))
      declarations.enter(first)
      declarations.enter(second)
      val canonicalPeers = List(first, second).map(semanticdbSymbol)
      assertEquals(canonicalPeers.distinct.size, 2)
      val comparisons = List(first, second).map { peer =>
        val orphan = owner.newMethodSymbol(TermName("selected"))
        orphan.setInfo(peer.info)
        val navigation = semanticdbSymbol(orphan)
        val serialized = orphan.toSemantic
        assertEquals(navigation, semanticdbSymbol(peer))
        assertEquals(
          Ident(TermName("selected")).setSymbol(orphan).toSemanticId.symbol,
          navigation
        )
        (navigation, serialized)
      }
      assertEquals(comparisons.map(_._2).distinct.size, 2)
      comparisons.foreach { case (navigation, serialized) =>
        assertEquals(serialized, navigation)
      }
      val unmatched = owner.newMethodSymbol(TermName("selected"))
      unmatched.setInfo(NullaryMethodType(definitions.DoubleTpe))
      assertEquals(semanticdbSymbol(unmatched), "")
      assertEquals(unmatched.toSemantic, "")
      assertEquals(
        Ident(TermName("selected")).setSymbol(unmatched).toSemanticId.symbol,
        ""
      )

      val duplicate = owner.newMethodSymbol(TermName("selected"))
      duplicate.setInfo(NullaryMethodType(definitions.StringTpe))
      declarations.enter(duplicate)
      val ambiguous = owner.newMethodSymbol(TermName("selected"))
      ambiguous.setInfo(duplicate.info)
      assertEquals(semanticdbSymbol(ambiguous), "")
      assertEquals(ambiguous.toSemantic, "")

      val cached = owner.newMethodSymbol(TermName("cached"))
      cached.setInfo(NullaryMethodType(definitions.IntTpe))
      declarations.enter(cached)
      val originalIdentity = cached.toSemantic
      declarations.unlink(cached)
      val replacementFirst = owner.newMethodSymbol(TermName("cached"))
      replacementFirst.setInfo(NullaryMethodType(definitions.StringTpe))
      val replacementSecond = owner.newMethodSymbol(TermName("cached"))
      replacementSecond.setInfo(cached.info)
      declarations.enter(replacementFirst)
      declarations.enter(replacementSecond)
      assertNotEquals(semanticdbSymbol(cached), originalIdentity)
      assertEquals(cached.toSemantic, semanticdbSymbol(replacementSecond))
    } finally {
      global.askShutdown()
    }
  }
}
