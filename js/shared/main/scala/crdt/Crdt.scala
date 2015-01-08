/**
 * Implementations taken from
 *
 * A comprehensive study of Convergent and Commutative Replicated Data Types
 * Marc Shapiro, Nuno Pregui¸ca, Carlos Baquero, Marek Zawirski
 */
package crdt
import collection.mutable

trait Basis{
  type PNCounter
  type LWWRegister[T >: Null]
}
trait StateBased[ThisType, ValueType]{
  def merge(other: ThisType): Unit
  def value: ValueType
}
object StateBased extends Basis{
  trait Counter[ThisType] extends StateBased[ThisType, Int]{
    def increment(n: Int): Unit
  }
  /**
   * Increment-only counter
   */
  class GCounter extends Counter[GCounter]{
    val shardId = util.Random.nextInt
    val counts = mutable.Map.empty[Int, Int].withDefaultValue(0)
    def increment(n: Int) = {
      assert(n >= 0, s"GCounters can only grow, increment $n is negative")
      counts(shardId) = counts(shardId) + n
    }
    def value = counts.valuesIterator.sum
    def merge(other: GCounter) = {
      for((k, v) <- other.counts){
        counts(k) = counts(k) max v
      }
    }
    override def toString = s"GCounter($value)"
  }

  /**
   * Increment-decrement counter
   */
  class PNCounter extends Counter[PNCounter]{
    val shardId = util.Random.nextInt
    val P = new GCounter
    val N = new GCounter
    def increment(n: Int) = {
      if (n > 0) P.increment(n)
      else if (n < 0) N.increment(-n)
    }
    def value = P.value - N.value
    def merge(other: PNCounter) = {
      P.merge(other.P)
      N.merge(other.N)
    }

    override def toString = s"PNCounter($value)"
  }

  /**
   * Writable cell with last-writer-wins concurrency
   */
  class LWWRegister[T](var v: T, var t: Long = System.nanoTime())
  extends StateBased[LWWRegister[T], T]{

    def assign(newT: T) = {
      v = newT
      t = System.nanoTime()
    }
    def value = v
    def merge(other: LWWRegister[T]) = {
      if (other.t > t){
        t = other.t
        v = other.v
      }
    }
    override def toString = s"LWWRegister($value)"
  }
  /**
   * Writable cell with multi-value merging
   */
  class MVRegister[T] {
    val v = mutable.Set.empty[(T, Int)]

    def value = v
//    def assign()
  }

  trait GSetLike[ThisType, T] extends StateBased[ThisType, Set[T]]{
    def add(t: T): Unit
  }

  /**
   * Set of items that can only grow
   */
  class GSet[T] extends GSetLike[GSet[T], T]{
    val v = mutable.Set.empty[T]
    def value = v.toSet
    def add(t: T) = v.add(t)
    def lookup(t: T) = v.contains(t)
    def merge(other: GSet[T]) = v ++= other.v
  }


  /**
   * Set of items where you can add and remove things, but
   * once-removed items cannot be re-added
   */
  class TwoPSet[T] extends GSetLike[TwoPSet[T], T]{
    val A = new GSet[T]
    val R = new GSet[T]
    def add(t: T) = A.add(t)
    def remove(t: T) = {
      assert(A.lookup(t), s"Can't remove $t that the TwoPSet hasn't seen")
      R.add(t)
    }
    def lookup(t: T) = A.lookup(t) && !R.lookup(t)
    def merge(other: TwoPSet[T]) = {
      A.merge(other.A)
      R.merge(other.R)
    }
    def value = A.value -- R.value
  }


}
trait Op[T]{
  def applyTo(t: T)
}
object OpBased extends Basis{
  object PNCounter{
    case class Increment(n: Int) extends Op[PNCounter]{
      def applyTo(t: PNCounter) = t.count += n
    }
  }
  class PNCounter{
    private var count = 0
    def value = count
  }

  object LWWRegister{
    case class Assign[T >: Null](value: T, ts: Long){
      def applyTo(t: LWWRegister[T]) = {
        if (ts > t.ts){
          t.ts = ts
          t.v = value
        }
      }
    }
  }
  /**
   * Writable cell with last-writer-wins concurrency
   */
  class LWWRegister[T >: Null]{
    private var v: T = null
    private var ts = 0L
    def value = v
  }

  /**
   * Set of items where you can add and remove things, but
   * once-removed items cannot be re-added
   */
  class TwoPSet[T]{
    val A = mutable.Set.empty[T]
    val R = mutable.Set.empty[T]
    def lookup(t: T) = A.contains(t) && !R.contains(t)

  }
  object TwoPSet{
    case class Add[T](t: T) extends Op[TwoPSet[T]]{
      def applyTo(crdt: TwoPSet[T]) = crdt.A.add(t)
    }
    case class Remove[T](t: T) extends Op[TwoPSet[T]]{
      def applyTo(crdt: TwoPSet[T]) = crdt.R.add(t)
    }
  }

  /**
   * A set where you can add or remove items, and an item
   * is in the set if it's removals > adds
   */
  class PNSet[T]{
    val S = mutable.Map.empty[T, Int].withDefaultValue(0)
    def lookup(t: T) = S(t) > 0
  }
  object PNSet{
    case class Add[T](t: T) extends Op[PNSet[T]]{
      def applyTo(crdt: PNSet[T]) = crdt.S(t) += 1
    }
    case class Remove[T](t: T) extends Op[PNSet[T]]{
      def applyTo(crdt: PNSet[T]) = crdt.S(t) -= 1
    }
  }

  /**
   * A set where you can only reove items you've seen
   */
  class ORSet[T]{
    val S = mutable.Set.empty[(T, Long)]
    def lookup(t: T) = S.exists(_._1 == t)

  }
  object ORSet{
    case class Add[T](t: T, unique: Long = util.Random.nextLong()) extends Op[ORSet[T]]{
      def applyTo(crdt: ORSet[T]) = crdt.S.add((t, unique))
    }
    case class Remove[T](t: T, unique: Long = util.Random.nextLong()) extends Op[ORSet[T]]{
      def applyTo(crdt: ORSet[T]) = {
        assert(crdt.S.contains((t, unique)))
        crdt.S.remove((t, unique))
      }
    }
  }

}






/**
 * Set where items are unique
 */
class USet

/**
 * Set where you can add and remove items, and the last
 * write to each individual element wins
 */
class LWWElementSet




/**
 * A directed graph that starts off with a start and an
 * end, where you can add edges and nodes only conforming
 * to the existing direction (start -> end) of the graph
 */
class AddOnlyMonotonicDag

/**
 * Like a AddOnlyMonotonicDag, but without the
 */
class AddRemovePartialOrder

/**
 * A sequence as a linked list, with addRight(v, a)
 */
class ReplicatedGrowableArray

class ContinuousSequence