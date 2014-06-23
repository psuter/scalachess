package chess

// This is a specialized data structures for maps indexed by Pos's.
// It takes advantage of the fact that there are most 64 keys to represent the key set
// as a single long.
class PosMap[T] private(mask: Long, array: IndexedSeq[Option[T]]) extends Map[Pos,T] {
  def +[S >: T](kv: (Pos,S)): PosMap[S] = {
    val (k,v) = kv
    new PosMap[S](mask | (1L << k.z), array.updated(k.z, Some(v)))
  }

  def -(k: Pos): PosMap[T] = {
    new PosMap[T](mask & ~(1L << k.z), array)
  }

  override def contains(k: Pos): Boolean = isDefinedAt(k)

  override def get(k: Pos): Option[T] = isDefinedAt(k).option(array(k.z).get)

  override def isDefinedAt(k: Pos): Boolean = ((mask & (1L << k.z)) != 0L)

  override def iterator: Iterator[(Pos,T)] = {
    // Sorry.
    var z = 0
    var l: List[(Pos,T)] = Nil
    
    while(z < 64) {
      if((mask & (1L << z)) != 0L)
        l = (Pos.all(z), array(z).get) :: l
      z += 1
    }

    l.iterator
  }

  override def keySet: Set[Pos] = iterator.map(_._1).toSet

  def mapValuesWithKey[S](f: ((Pos,T)) => S): PosMap[S] = {
    new PosMap[S](mask, Pos.all.zip(array).map {
      case (k,v) => if(contains(k)) Some(f((k,v.get))) else None
    })
  }

  override def values: Iterable[T] = iterator.map(_._2).toIterable

  override def filter(pred: ((Pos,T))=>Boolean): PosMap[T] = {
    var newMask: Long = mask
    var z = 0

    while(z < 64) {
      if((mask & (1L << z)) != 0L && !pred(Pos.all(z), array(z).get))
        newMask &= ~(1L << z)
      z += 1
    }
    new PosMap[T](newMask, array)
  }

  def map[U,V](f: (Pos,T)=>(U,V)): Map[U,V] = iterator.map { p =>
    f(p._1, p._2)
  } toMap
}

object PosMap {
  def apply[T](elems: Traversable[(Pos,T)]): PosMap[T] =
    elems.foldLeft(PosMap.empty[T])(_ + _)

  def empty[T]: PosMap[T] = new PosMap[T](0L, IndexedSeq.fill(64)(None))
}
