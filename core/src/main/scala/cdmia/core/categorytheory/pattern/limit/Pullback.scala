package cdmia.core.categorytheory.pattern.limit

import cdmia.core.categorytheory
import cdmia.core.categorytheory.{Category, Config, Object}
import cdmia.core.categorytheory.functor.Functor
import cdmia.core.categorytheory.morphism.{Morphism, MorphismComposition}

/**
 * A pullback is a limit having the following shape and that commutes:
 *
 *      vertex --> b
 *        ↓        ↓
 *        a -----> c
 *
 * @param _vertex the [[categorytheory.Object]] that is the vertex of this pullback.
 * @param a       the first [[categorytheory.Object]] connected to the vertex.
 * @param b       the second [[categorytheory.Object]] connected to the vertex.
 * @param c       the [[categorytheory.Object]] connected to the objects a and b.
 * @param va      the [[Morphism]] from the vertex to a.
 * @param vb      the [[Morphism]] from the vertex to b.
 * @param ac      the [[Morphism]] from a to c.
 * @param bc      the [[Morphism]] from b to c.
 */
class Pullback(_vertex: categorytheory.Object, val a: categorytheory.Object, val b: categorytheory.Object, val c: categorytheory.Object,
               val va: Morphism, val vb: Morphism, val ac: Morphism, val bc: Morphism,
               name: String = "Pullback") extends Limit(_vertex, name) {
  if (!Config.disableRequire) {
    require(va.domain == vertex && va.codomain == a, s"va must be $vertex -> $a, but got ${va.domain} -> ${va.codomain}.")
    require(vb.domain == vertex && vb.codomain == b, s"vb must be $vertex -> $b, but got ${vb.domain} -> ${vb.codomain}.")
    require(ac.domain == a && ac.codomain == c, s"ac must be $a -> $c, but got ${ac.domain} -> ${ac.codomain}.")
    require(bc.domain == b && bc.codomain == c, s"bc must be $b -> $c, but got ${bc.domain} -> ${bc.codomain}.")
  }

  override protected val objectsToCheck: Iterable[categorytheory.Object] = Iterable[categorytheory.Object](a, b)
  override protected val morphismsToCheck: Map[categorytheory.Object, Morphism] = Map[categorytheory.Object, Morphism](a -> va, b -> vb)

  override def getObjects: List[categorytheory.Object] = List[categorytheory.Object](vertex, a, b, c)

  override def getMorphisms: List[Morphism] = List[Morphism](va, vb, ac, bc)

  /**
   * For a pullback to be valid in a category, all the objects and morphisms must be in the category, and the diagram must
   * commute.
   *
   * @param category the [[Category]] in which to check the validity of this pullback.
   * @return true is this pullback is valid, false otherwise.
   */
  override def isValid(category: Category): Boolean = {
    category.objects.exists(_ == a) && category.objects.exists(_ == b) && category.objects.exists(_ == c) &&
      category.objects.exists(_ == vertex) &&
      category.existsMorphism(va) && category.existsMorphism(vb) &&
      category.existsMorphism(ac) && category.existsMorphism(bc) &&
      category.areEqual(ac o va, bc o vb)
  }

  override def explainIsNotValid(category: Category): List[String] = {
    var invalidityReasons: List[String] = List[String]()
    for (obj <- List[categorytheory.Object](vertex, a, b, c).distinct if !category.objects.exists(_ == obj)) {
      invalidityReasons :+= s"The object $obj does not exist in the category ${category.name}."
    }
    for (morphism <- List[Morphism](va, vb, ac, bc).distinct if !category.existsMorphism(morphism)) {
      invalidityReasons :+= s"The Morphism $morphism does not exist in the category ${category.name}."
    }
    if (invalidityReasons.isEmpty && !category.areEqual(ac o va, bc o vb)) {
      invalidityReasons :+= s"${ac o va} is not equal to ${bc o vb} in the category ${category.name}."
    }
    invalidityReasons
  }

  override def createPatternInDestinationCategory(functor: Functor): Limit = {
    val vertexTransformation = functor.getDestinationObject(vertex)
    val aTransformation = functor.getDestinationObject(a)
    val bTransformation = functor.getDestinationObject(b)
    val cTransformation = functor.getDestinationObject(c)
    val vaTransformation = functor.getDestinationMorphism(va)
    val vbTransformation = functor.getDestinationMorphism(vb)
    val acTransformation = functor.getDestinationMorphism(ac)
    val bcTransformation = functor.getDestinationMorphism(bc)
    new Pullback(vertexTransformation, aTransformation, bTransformation, cTransformation,
      vaTransformation, vbTransformation, acTransformation, bcTransformation)
  }

  override def createPatternsInSourceCategory(functor: Functor): List[Limit] = {
    def getSourceMorphismInComposition(morphism: Morphism): List[Morphism] = {
      functor.morphismTransformations.filter(mt => {
        val allMorphismsOfPullback = getMorphisms.flatMap(m => m match
          case composition: MorphismComposition => composition.chainOfMorphisms
          case _ => List(m)
        )
        mt.destination.isInstanceOf[MorphismComposition] &&
          mt.destination.asInstanceOf[MorphismComposition].chainOfMorphisms.contains(morphism) //&&
          //mt.destination.asInstanceOf[MorphismComposition].chainOfMorphisms.forall(m => allMorphismsOfPullback.contains(m))
      }).map(_.source).toList
    }

    val vertexOrigins = functor.getSourceObjects(vertex)
    val aOrigins = functor.getSourceObjects(a)
    val bOrigins = functor.getSourceObjects(b)
    val cOrigins = functor.getSourceObjects(c)
    val vaOrigins = functor.getSourceMorphisms(va) ::: getSourceMorphismInComposition(va)
    val vbOrigins = functor.getSourceMorphisms(vb) ::: getSourceMorphismInComposition(vb)
    val acOrigins = functor.getSourceMorphisms(ac) ::: getSourceMorphismInComposition(ac)
    val bcOrigins = functor.getSourceMorphisms(bc) ::: getSourceMorphismInComposition(bc)

    // Start with vertex
    var partialPatterns = vertexOrigins.map(o => new Pullback(o, a, b, c, new Morphism(va.name, o, a), new Morphism(vb.name, o, b), ac, bc))

    // Add va and isolated a
    partialPatterns = partialPatterns.filter(p => vaOrigins.exists(_.domain == p.vertex)).flatMap(p => {
      vaOrigins.filter(_.domain == p.vertex).map(m => new Pullback(p.vertex, m.codomain, p.b, p.c, m, p.vb, new Morphism(p.ac.name, m.codomain, p.ac.codomain), p.bc))
    }) ::: partialPatterns.filterNot(p => vaOrigins.exists(_.domain == p.vertex))
    partialPatterns :::= aOrigins.filterNot(partialPatterns.map(_.a).contains(_)).map(o => new Pullback(vertex, o, b, c, new Morphism(va.name, vertex, o), vb, new Morphism(ac.name, o, c), bc))

    // Add vb and isolated b
    partialPatterns = partialPatterns.filter(p => vbOrigins.exists(_.domain == p.vertex)).flatMap(p => {
      vbOrigins.filter(_.domain == p.vertex).map(m => new Pullback(p.vertex, p.a, m.codomain, p.c, p.va, m, p.ac, new Morphism(p.bc.name, m.codomain, p.c)))
    }) :::
      partialPatterns.filterNot(p => vbOrigins.exists(_.domain == p.vertex))
    partialPatterns :::= bOrigins.filterNot(partialPatterns.map(_.b).contains(_)).map(o => new Pullback(vertex, a, o, c, va, new Morphism(vb.name, vertex, o), ac, new Morphism(bc.name, o, c)))

    // Add ac connected to previously computed patterns and isolated ac
    partialPatterns = partialPatterns.filter(p => acOrigins.exists(_.domain == p.a)).flatMap(p => {
      acOrigins.filter(_.domain == p.a).map(m => new Pullback(p.vertex, p.a, p.b, m.codomain, p.va, p.vb, m, new Morphism(p.bc.name, p.b, m.codomain)))
    }) ::: partialPatterns.filterNot(p => acOrigins.exists(_.domain == p.a))
    partialPatterns :::= acOrigins.filterNot(m => partialPatterns.map(_.ac).contains(m)).map(m =>
        new Pullback(vertex, m.domain, b, m.codomain, new Morphism(va.name, vertex, m.domain), vb, m, new Morphism(bc.name, b, m.codomain))
      )

    // Add bc and diverging bc from ac connected to previously computed patterns and isolated bc
    partialPatterns = partialPatterns.filter(p => bcOrigins.exists(m => m.domain == p.b && m.codomain == p.c)).flatMap(p => {
      bcOrigins.filter(m => m.domain == p.b && m.codomain == p.c).map(m => new Pullback(p.vertex, p.a, p.b, p.c, p.va, p.vb, p.ac, m))
    }) :::
      partialPatterns.filter(p => bcOrigins.exists(m => m.domain == p.b && m.codomain != p.c)).flatMap(p => {
        bcOrigins.filter(m => m.domain == p.b && m.codomain != p.c).flatMap(m => {
          List(
            new Pullback(p.vertex, p.a, m.domain, m.codomain, p.va, new Morphism(p.vb.name, p.vertex, m.domain), new Morphism(p.ac.name, p.a, m.codomain), m),
            p
          )
        })
      }) ::: partialPatterns.filterNot(p => bcOrigins.exists(m => m.domain == p.b))
    partialPatterns :::= bcOrigins.filterNot(m => partialPatterns.map(_.bc).contains(m)).map(m =>
        new Pullback(vertex, a, m.domain, m.codomain, va, new Morphism(vb.name, vertex, m.domain), new Morphism(ac.name, a, m.codomain), m)
      )

    // Add isolated c
    partialPatterns :::= cOrigins.filterNot(o => partialPatterns.map(_.c).contains(o)).map(o =>
      new Pullback(vertex, a, b, o, va, vb, new Morphism(ac.name, ac.domain, o), new Morphism(bc.name, bc.domain, o))
    )

    partialPatterns
  }

  override def equals(obj: Any): Boolean = obj match
    case pullback: Pullback => pullback.vertex == this.vertex && pullback.a == this.a && pullback.b == this.b && pullback.c == this.c &&
      pullback.va == this.va && pullback.vb == this.vb && pullback.ac == this.ac && pullback.bc == this.bc
    case _ => false
}
