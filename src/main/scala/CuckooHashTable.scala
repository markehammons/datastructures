import CuckooHashTable.{Backing}
import scala.annotation.tailrec

class CuckooHashTable[Key, Value](val backing: Backing[Key, Value], sundaramPos: Int) {

  final val backing1 = backing._1
  final val backing2 = backing._2

  val reserved = backing1.size

  lazy val size = backing1.count(_ != null) + backing2.count(_ != null)

  def insert(kv: (Key, Value)): CuckooHashTable[Key, Value] = {
    if(kv != null)
      this.insert(kv._1, kv._2)
    else
      this
  }

  def insert(k: Key, v: Value): CuckooHashTable[Key, Value] = {
    val pos = CuckooHashTable.hash1(k.hashCode(), reserved)
    val elem = backing1(pos)

    if(elem == null || elem._1.equals(k)) {
      //println(s"Inserting ${(k,v)} into table")
      new CuckooHashTable((backing1.updated(pos, (k,v)), backing2), sundaramPos)
    } else {
      //println(s"Collision while trying to insert ${(k,v)}")
      insertHelper((k,v), Set.empty, backing, false) map (new CuckooHashTable(_, sundaramPos)) getOrElse rebuild.insert(k,v)
    }
  }

  @tailrec
  private def insertHelper(kv: (Key, Value), moved: Set[Key], nBacking: Backing[Key, Value], moveToFirst: Boolean): Option[Backing[Key, Value]] = {

    var pos = 0
    var bU: BackingUpdater[Key, Value] = null

    if(!moveToFirst) {
      pos = CuckooHashTable.hash2(kv._1.hashCode(), reserved)
      bU = RightUpdater(nBacking)
    } else {
      pos = CuckooHashTable.hash1(kv._1.hashCode(), reserved)
      bU = LeftUpdater(nBacking)
    }

    val old = bU(pos)

    if(old == null || old._1 == kv._1)
      Some(bU.updated(pos, kv))
    else if(!moved.contains(old._1))
      insertHelper(old, moved + kv._1, bU.updated(pos, kv), !moveToFirst)
    else
      None
  }

  def get(k: Key): Value = {
    val ret = backing1(CuckooHashTable.hash1(k.hashCode(), reserved))

    if(ret._1 != k)
      backing2(CuckooHashTable.hash2(k.hashCode(), reserved))._2
    else
      ret._2
  }

  @inline
  private def checkPos(i: Int, k: Key) = {
    val elem = backing1(i)
    elem == null || elem._1.equals(k)
  }

  def rebuild: CuckooHashTable[Key, Value] = {
    val nLen = CuckooHashTable.primes(sundaramPos)
    //println(s"Rebuild! New table size: $nLen")
    //println(s"Size/Reserved of current table: $size/$reserved")


    val vecBuilder = Vector.newBuilder[(Key, Value)]
    //val nVec = Vector.fill(nLen)(null.asInstanceOf[(Key, Value)])

    var i = 0
    while(i < nLen)  {
      vecBuilder += null
      i += 1
    }

    val nVec = vecBuilder.result()
    vecBuilder.clear()

    rebuildHelper(0, new CuckooHashTable((nVec,nVec),sundaramPos + 1))

  }

  @tailrec
  private def rebuildHelper(pos: Int, nHash: CuckooHashTable[Key,Value]): CuckooHashTable[Key, Value] = {
    if(pos < reserved) {
      rebuildHelper(pos + 1, nHash.insert(backing1(pos)).insert(backing2(pos)))
    } else nHash
  }

  override def toString = s"CuckooHashTable(${backing1.mkString(", ")}, ${backing2.mkString(", ")})"
}


trait BackingUpdater[Key,Value] extends {
  def backing: Backing[Key,Value]
  def updated(pos: Int, kv: (Key,Value)): Backing[Key,Value]
  def apply(pos: Int): (Key,Value)
}

final case class LeftUpdater[K,V](backing: Backing[K,V]) extends BackingUpdater[K,V] {
  def updated(pos: Int, kv: (K,V)) = (backing._1.updated(pos, kv), backing._2)
  def apply(pos: Int) = backing._1(pos) 
}

final case class RightUpdater[K,V](backing: Backing[K,V]) extends BackingUpdater[K,V] {
  def updated(pos: Int, kv: (K,V)) = (backing._1, backing._2.updated(pos, kv))
  def apply(pos: Int) = backing._2(pos)
}

object CuckooHashTable {
  val growthRatio = 1.5
  private val absMask = 0x7FFFFFFF

  type Backing[Key,Value] = (Vector[(Key, Value)], Vector[(Key, Value)])
  type Pair[A,B] = (A,B)



  def apply[Key,Value]() = {
    val empty = Vector(null, null)
    new CuckooHashTable[Key, Value]((empty, empty), 1)
  }

  def hash2(i: Int, len: Int): Int = {
    val y = i + 1 + ((i >> 31) << 1)
    val z = y - (y >> 31)
    (z & absMask) % len
  }

  def baseHash(i: Int): Int = {
    var a = (i ^ 61) ^ (i >>> 16)
    a += (a << 3)
    a ^= (a >>> 4)
    a *= 0x27d4eb2d
    (a ^ (a >>> 15))
  }

  def oHash(i: Int): Int = {
    val h = (i >>> 20) ^ (i >>> 12)
    (h ^ (h >>> 7) ^ (h >>> 4))
  }

  def oHash2(i: Int) = {
    var x = ((i >>> 16) ^ i) * 0x45d9f3b
    x = ((x >> 16) ^ x) * 0x45d9f3b
    ((x >> 16) ^ x)
  }

  final def xorShift32(i: Int) = {
    var a = i ^ (i << 13)
    a ^= (a >>> 17)
    a ^ (a << 5)
  }

  def hash1(i: Int, len: Int): Int = (xorShift32(i) & absMask) % len//math.abs(baseHash(i) % backing.length)

  def sundaram(primesBelow: Int) = {
    //val sqrt is a workaround for a segfault issue in scala 2.9.1
    val n = (primesBelow - 2)/2
    val nonPrimes = (for (i <- 1 to n; j <- i to (n - i) / (2 * i + 1)) yield i+j+(2*i*j)).toSet
    2 +:((1 to n) filterNot (nonPrimes.contains(_)) map (2*_+1))
  }

  val primes = (sundaram(12000000) foldLeft Vector(2)) { case (nVec, prime) => if(prime.toDouble / nVec.last >= growthRatio) nVec :+ prime else nVec }

  //def divHash(i: Int, len: Int): Int = math.abs(i / divVal % len)
}

