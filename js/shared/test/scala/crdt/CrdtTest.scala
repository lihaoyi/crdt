package crdt

import crdt.StateBased._
import utest._
object CrdtTest extends TestSuite{
  val tests = TestSuite{

    class Values[T <: StateBased[T, V], V](r1: T, r2: T, r3: T){
      def apply(v1: V, v2: V, v3: V) = assert(
        r1.value == v1,
        r2.value == v2,
        r3.value == v3
      )
    }


    def CounterTest[T <: Counter[T]](c1: T, c2: T, c3: T) = {
      val values = new Values[T, Int](c1, c2, c3)

      c1.increment(1)
      c2.increment(2)
      c3.increment(4)
      // Initial result works
      values(1, 2, 4)

      // Merging works
      c1.merge(c2)
      c2.merge(c3)
      values(3, 6, 4)
      // Merging redundantly does nothing
      c2.merge(c3)
      values(3, 6, 4)

      // But merging which spreads gossip works
      c1.merge(c2)
      values(7, 6, 4)
      c3.merge(c2)
      values(7, 6, 6)
      c3.merge(c1)
      c2.merge(c3)
      values(7, 7, 7)
    }

    'GCounter{
      'Pass - CounterTest[GCounter](new GCounter, new GCounter, new GCounter)

      'Fail{
        val c1 = new GCounter
        intercept[java.lang.AssertionError]{ c1.increment(-1) }
        intercept[java.lang.AssertionError]{ c1.increment(Int.MinValue) }
      }
    }
    'PNCounter{
      'Grow - CounterTest[PNCounter](new PNCounter, new PNCounter, new PNCounter)
      'Mixed - {
        val (c1, c2, c3) = (new PNCounter, new PNCounter, new PNCounter)
        val values = new Values[PNCounter, Int](c1, c2, c3)
        c1.increment(1)
        c2.increment(-1)
        c3.increment(10)
        values(1, -1, 10)
        // Merging works
        c1.merge(c2)
        c2.merge(c3)
        values(0, 9, 10)
        // Redundant merging
        c1.merge(c2)
        c2.merge(c3)
        values(10, 9, 10)
        c3.merge(c1)
        values(10, 9, 10)
        // Dumping a bunch of stuff even when things aren't settled works
        c1.increment(-20)
        c2.increment(-40)
        values(-10, -31, 10)
        c3.merge(c1)
        values(-10, -31, -10)
        // Eventually, when everyone merges, it reaches consistency
        c3.merge(c1)
        c2.merge(c1)
        values(-10, -50, -10)
        c1.merge(c2)
        c3.merge(c2)
        values(-50, -50, -50)
      }
    }
    'LWWRegister{
      val r1 = new LWWRegister[Int](0)
      val r2 = new LWWRegister[Int](1)
      val r3 = new LWWRegister[Int](2)
      val values = new Values[LWWRegister[Int], Int](r1, r2, r3)
      values(0, 1, 2)
      // backwards in time does nothing
      r3.merge(r2)
      r2.merge(r1)
      r3.merge(r1)
      values(0, 1, 2)

      // Forwards in time updates
      r1.merge(r2)
      r2.merge(r3)
      values(1, 2, 2)
      r1.merge(r3)
      values(2, 2, 2)

      // Updating someone makes him take over since he's the newest
      r2.assign(10)
      r2.merge(r1)
      r2.merge(r3)
      values(2, 10, 2)
      r1.merge(r2)
      r3.merge(r2)
      values(10, 10, 10)
    }
    def GSetTest[T <: GSetLike[T, Int]](s1: T, s2: T, s3: T) = {
      val values = new Values[T, Set[Int]](s1, s2, s3)
      values(Set(), Set(), Set())
      s1.add(1)
      values(Set(1), Set(), Set())
      s2.add(2)
      values(Set(1), Set(2), Set())
      s1.merge(s2)
      values(Set(1, 2), Set(2), Set())
      s3.merge(s1)
      values(Set(1, 2), Set(2), Set(1, 2))
      s3.merge(s2)
      values(Set(1, 2), Set(2), Set(1, 2))
      s2.merge(s3)
      values(Set(1, 2), Set(1, 2), Set(1, 2))
    }
    'GSet - GSetTest(new GSet[Int], new GSet[Int], new GSet[Int])
    'TwoPSet {
      'Grow - GSetTest(new TwoPSet[Int], new TwoPSet[Int], new TwoPSet[Int])
      'Both {
        val (s1, s2, s3) = (new TwoPSet[Int], new TwoPSet[Int], new TwoPSet[Int])
        val values = new Values[TwoPSet[Int], Set[Int]](s1, s2, s3)
        values(Set(), Set(), Set())
        s1.add(1)
        values(Set(1), Set(), Set())
        s2.add(2)
        values(Set(1), Set(2), Set())
        s1.merge(s2)
        values(Set(1, 2), Set(2), Set())
        s3.merge(s1)
        values(Set(1, 2), Set(2), Set(1, 2))
        s3.merge(s2)
        values(Set(1, 2), Set(2), Set(1, 2))
        s2.merge(s3)
        values(Set(1, 2), Set(1, 2), Set(1, 2))
        s1.remove(1)
        s2.remove(1)
        s3.remove(1)
        values(Set(2), Set(2), Set(2))
        s1.remove(2)
        values(Set(), Set(2), Set(2))
        s1.merge(s2)
        values(Set(), Set(2), Set(2))
        s2.merge(s1)
        values(Set(), Set(), Set(2))
        s3.merge(s2)
        values(Set(), Set(), Set())
      }

      'Failure{
        val (s1, s2, s3) = (new TwoPSet[Int], new TwoPSet[Int], new TwoPSet[Int])
        val values = new Values[TwoPSet[Int], Set[Int]](s1, s2, s3)
        intercept[java.lang.AssertionError]{ s1.remove(1) }
      }
    }
  }
}
