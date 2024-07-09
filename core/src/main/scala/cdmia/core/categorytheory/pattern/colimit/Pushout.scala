package cdmia.core.categorytheory.pattern.colimit

import cdmia.core.categorytheory
import cdmia.core.categorytheory.{Category, Object}
import cdmia.core.categorytheory.functor.Functor
import cdmia.core.categorytheory.morphism.{Morphism, MorphismComposition}

/**
 * A pushout is a limit having the following shape and that commutes:
 *
 *        c -----> b
 *        ↓        ↓
 *        a -----> vertex
 *
 * @param _vertex the [[categorytheory.Object]] that is the vertex of this pushout.
 * @param a       the first [[categorytheory.Object]] connected to the vertex.
 * @param b       the second [[categorytheory.Object]] connected to the vertex.
 * @param c       the [[categorytheory.Object]] connected to the objects a and b.
 * @param av      the [[Morphism]] from a to the vertex.
 * @param bv      the [[Morphism]] from b to the vertex.
 * @param ca      the [[Morphism]] from c to a.
 * @param cb      the [[Morphism]] from c to b.
 */
class Pushout(_vertex: categorytheory.Object, val a: categorytheory.Object, val b: categorytheory.Object, val c: categorytheory.Object,
              val av: Morphism, val bv: Morphism, val ca: Morphism, val cb: Morphism,
              name: String = "Pushout") extends Colimit(_vertex, name) {
  require(av.codomain == vertex && av.domain == a, s"av must be $a -> $vertex, but got ${av.domain} -> ${av.codomain}.")
  require(bv.codomain == vertex && bv.domain == b, s"bv must be $b -> $_vertex, but got ${bv.domain} -> ${bv.codomain}.")
  require(ca.codomain == a && ca.domain == c, s"ca must be $c -> $a, but got ${ca.domain} -> ${ca.codomain}.")
  require(cb.codomain == b && cb.domain == c, s"cb must be $c -> $b, but got ${cb.domain} -> ${cb.codomain}.")

  override protected val objectsToCheck: Iterable[categorytheory.Object] = Iterable[categorytheory.Object](a, b)
  override protected val morphismsToCheck: Map[categorytheory.Object, Morphism] = Map[categorytheory.Object, Morphism](a -> av, b -> bv)

  override def getObjects: List[categorytheory.Object] = List[categorytheory.Object](vertex, a, b, c)

  override def getMorphisms: List[Morphism] = List[Morphism](av, bv, ca, cb)

  /**
   * For a pushout to be valid in a category, all the objects and morphisms must be in the category, and the diagram must
   * commute.
   *
   * @param category the [[Category]] in which to check the validity of this pullback.
   * @return true is this pullback is valid, false otherwise.
   */
  override def isValid(category: Category): Boolean = {
    category.objects.exists(_ == a) && category.objects.exists(_ == b) && category.objects.exists(_ == c) &&
      category.objects.exists(_ == vertex) &&
      category.existsMorphism(av) && category.existsMorphism(bv) &&
      category.existsMorphism(ca) && category.existsMorphism(cb) &&
      category.areEqual(av o ca, bv o cb)
  }

  override def explainIsNotValid(category: Category): List[String] = {
    var invalidityReasons: List[String] = List[String]()
    for (obj <- List[categorytheory.Object](vertex, a, b, c).distinct if !category.objects.exists(_ == obj)) {
      invalidityReasons :+= s"The object $obj does not exist in the category."
    }
    for (morphism <- List[Morphism](av, bv, ca, cb).distinct if !category.existsMorphism(morphism)) {
      invalidityReasons :+= s"The Morphism $morphism does not exist in the category."
    }
    if (!category.areEqual(av o ca, bv o cb)) {
      invalidityReasons :+= s"${av o ca} is not equal to ${bv o cb}."
    }
    invalidityReasons
  }

  override def createPatternInDestinationCategory(functor: Functor): Colimit = {
    val vertexTransformation = functor.getDestinationObject(vertex)
    val aTransformation = functor.getDestinationObject(a)
    val bTransformation = functor.getDestinationObject(b)
    val cTransformation = functor.getDestinationObject(c)
    val avTransformation = functor.getDestinationMorphism(av)
    val bvTransformation = functor.getDestinationMorphism(bv)
    val caTransformation = functor.getDestinationMorphism(ca)
    val cbTransformation = functor.getDestinationMorphism(cb)
    new Pushout(vertexTransformation, aTransformation, bTransformation, cTransformation,
      avTransformation, bvTransformation, caTransformation, cbTransformation)
  }

  override def createPatternsInSourceCategory(functor: Functor): List[Colimit] = {
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
    val avOrigins = functor.getSourceMorphisms(av) ::: getSourceMorphismInComposition(av)
    val bvOrigins = functor.getSourceMorphisms(bv) ::: getSourceMorphismInComposition(bv)
    val caOrigins = functor.getSourceMorphisms(ca) ::: getSourceMorphismInComposition(ca)
    val cbOrigins = functor.getSourceMorphisms(cb) ::: getSourceMorphismInComposition(cb)

    // Start with vertex
    var partialPatterns = vertexOrigins.map(o => new Pushout(o, a, b, c, new Morphism(av.name, a, o), new Morphism(bv.name, b, o), ca, cb))

    // Add va and isolated a
    partialPatterns = partialPatterns.filter(p => avOrigins.exists(_.codomain == p.vertex)).flatMap(p => {
      avOrigins.filter(_.codomain == p.vertex).map(m => new Pushout(p.vertex, m.domain, p.b, p.c, m, p.bv, new Morphism(p.ca.name, p.c, m.domain), p.cb))
    }) ::: partialPatterns.filterNot(p => avOrigins.exists(_.codomain == p.vertex))
    partialPatterns :::= aOrigins.filterNot(partialPatterns.map(_.a).contains(_)).map(o => new Pushout(vertex, o, b, c, new Morphism(av.name, o, vertex), bv, new Morphism(ca.name, c, o), cb))

    // Add bv and isolated b
    partialPatterns = partialPatterns.filter(p => bvOrigins.exists(_.codomain == p.vertex)).flatMap(p => {
      bvOrigins.filter(_.codomain == p.vertex).map(m => new Pushout(p.vertex, p.a, m.domain, p.c, p.av, m, p.ca, new Morphism(p.cb.name, p.c, m.domain)))
    }) ::: partialPatterns.filterNot(p => bvOrigins.exists(_.codomain == p.vertex))
    partialPatterns :::= bOrigins.filterNot(partialPatterns.map(_.b).contains(_)).map(o => new Pushout(vertex, a, o, c, av, new Morphism(bv.name, o, vertex), ca, new Morphism(cb.name, c, o)))

    // Add ca connected to previously computed patterns and isolated ca
    partialPatterns = partialPatterns.filter(p => caOrigins.exists(_.codomain == p.a)).flatMap(p => {
      caOrigins.filter(_.codomain == p.a).map(m => new Pushout(p.vertex, p.a, p.b, m.domain, p.av, p.bv, m, new Morphism(p.cb.name, m.domain, p.b)))
    }) ::: partialPatterns.filterNot(p => caOrigins.exists(_.codomain == p.a))
    partialPatterns :::= caOrigins.filterNot(m => partialPatterns.map(_.ca).contains(m)).map(m =>
        new Pushout(vertex, m.codomain, b, m.domain, new Morphism(av.name, m.codomain, vertex), bv, m, new Morphism(cb.name, m.domain, b))
      )

    // Add cb and diverging bc from ca connected to previously computed patterns and isolated cb
    partialPatterns = partialPatterns.filter(p => cbOrigins.exists(m => m.codomain == p.b && m.domain == p.c)).flatMap(p => {
      cbOrigins.filter(m => m.codomain == p.b && m.domain == p.c).map(m => new Pushout(p.vertex, p.a, p.b, p.c, p.av, p.bv, p.ca, m))
    }) :::
      partialPatterns.filter(p => cbOrigins.exists(m => m.codomain == p.b && m.domain != p.c)).flatMap(p => {
        cbOrigins.filter(m => m.codomain == p.b && m.domain != p.c).flatMap(m => {
          List(
            new Pushout(p.vertex, p.a, m.codomain, m.domain, p.av, new Morphism(p.bv.name, m.codomain, p.vertex), new Morphism(p.ca.name, m.domain, p.a), m),
            p
          )
        })
      }) ::: partialPatterns.filterNot(p => cbOrigins.exists(m => m.codomain == p.b))
    partialPatterns :::= cbOrigins.filterNot(m => partialPatterns.map(_.cb).contains(m)).map(m =>
        new Pushout(vertex, a, m.codomain, m.domain, av, new Morphism(bv.name, m.codomain, vertex), new Morphism(ca.name, m.domain, a), m)
      )

    // Add isolated c
    partialPatterns :::= cOrigins.filterNot(o => partialPatterns.map(_.c).contains(o)).map(o =>
      new Pushout(vertex, a, b, o, av, bv, new Morphism(ca.name, o, a), new Morphism(cb.name, o, b)))

    partialPatterns
  }

  override def equals(obj: Any): Boolean = obj match
    case pushout: Pushout => pushout.vertex == this.vertex && pushout.a == this.a && pushout.b == this.b && pushout.c == this.c &&
      pushout.av == this.av && pushout.bv == this.bv && pushout.ca == this.ca && pushout.cb == this.cb
    case _ => false
}
