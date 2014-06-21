package chess

// Throughout this file, `z` denotes a Zobrist hash.
trait SituationGraph[E,T] {
  def nodeCount: Int

  def nodes: Iterable[(Long,T)]

  def isDefinedAt(z: Long): Boolean

  def get(z: Long): Option[T]

  def withNode(z: Long, info: T): SituationGraph[E,T]

  def withEdge(zFrom: Long, zTo: Long, label: E): SituationGraph[E,T]

  def outEdges(z: Long): Option[List[E]]

  def followFrom(z: Long, label: E): Option[Long]
}

object SituationGraph {
  def empty[E,T] : SituationGraph[E,T] = new SGImpl[E,T](Set.empty, Map.empty, Map.empty)

  private class SGImpl[E,T](
    val zs: Set[Long],
    val ns: Map[Long,T],
    val es: Map[Long,Map[E,Long]]) extends SituationGraph[E,T] {
    
    val nodeCount: Int = ns.size 

    def nodes: Iterable[(Long,T)] = ns.toIterable

    def isDefinedAt(z: Long): Boolean = zs(z)

    def get(z: Long): Option[T] = ns.get(z)

    def withNode(z: Long, info: T): SituationGraph[E,T] = {
      new SGImpl(zs + z, ns + (z -> info), es)
    }

    def withEdge(zFrom: Long, zTo: Long, label: E): SituationGraph[E,T] = {
      assert(zs(zFrom) && zs(zTo))
      val newOutEdges: Map[E,Long] = es.getOrElse(zFrom, Map.empty).updated(label, zTo)
      new SGImpl(zs, ns, es.updated(zFrom, newOutEdges)) 
    }

    def outEdges(z: Long): Option[List[E]] = es.get(z) map { m => m.keySet.toList }

    def followFrom(z: Long, label: E): Option[Long] = es.get(z).flatMap(m => m.get(label))
  }
}

