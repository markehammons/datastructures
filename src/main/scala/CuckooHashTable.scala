/**
 * Created with IntelliJ IDEA.
 * User: luft
 * Date: 1/29/14
 * Time: 11:40 AM
 */
class CuckooHashTable[Key,Value](val backing: Vector[(Key, Value)], val sundaramStep: Int) {
  def insert(k: Key, v: Value): CuckooHashTable[Key, Value] = {

    val pos = CuckooHashTable.hash1(k.hashCode(), backing.length)


    def moveToSecond(kv: (Key, Value), moved: Set[Key], nBacking: Vector[(Key,Value)]): Vector[(Key, Value)] = {
      val pos = CuckooHashTable.hash2(kv._1.hashCode(), nBacking.length)

      //println(s"I have $kv")
      //println(s"Gonna replace $pos")
      //println(nBacking)

      if(nBacking(pos) == null || nBacking(pos)._1 == kv._1) {
        //println(s"Sucessfully inserted $kv.")
        nBacking.updated(pos,kv)
      } else {
        if(moved.contains(nBacking(pos)._1)) {
          //println(s"$moved contains ${nBacking(pos)._1}")
          //println(s"${moved.map(k => CuckooHashTable.hash2(k.hashCode(), nBacking.length))} vs ${(pos, kv._1)}")
          Vector.empty
        } else {
          //println(s"Attempting to replace ${nBacking(pos)} with $kv.")
          moveToSecond(nBacking(pos), moved + kv._1, nBacking.updated(pos, kv))
        }
      }
    }

    if(checkPos(pos, k)) {
      //println(s"Inserting ${(k,v)} into table")
      new CuckooHashTable(backing.updated(pos, (k,v)), sundaramStep)
    } else {
      //println(s"Collision while trying to insert ${(k,v)}")

      val altVec = moveToSecond((k,v), Set.empty, backing)

      if(altVec.isEmpty) {
        //println("Rebuilding table...")
        rebuild.insert(k,v)
      } else {
        new CuckooHashTable(altVec, sundaramStep)
      }
    }
  }

  def get(k: Key): Value = {
    val ret = backing(CuckooHashTable.hash1(k.hashCode(), backing.length))

    if(ret._1 != k)
      backing(CuckooHashTable.hash2(k.hashCode(), backing.length))._2
    else
      ret._2
  }

  def checkPos(i: Int, k: Key) = backing(i) == null || backing(i)._1 == k

  def rebuild: CuckooHashTable[Key, Value] = {
    val nLen = CuckooHashTable.primes(sundaramStep)

    val builder = new CuckooHashTable(Vector.fill[(Key,Value)](nLen)(null.asInstanceOf[(Key, Value)]), sundaramStep + 1)

    backing.foldLeft(builder){case (nTable, value) => if(value == null) nTable else nTable.insert(value._1, value._2)}
  }

  override def toString = (backing.foldLeft("CuckooHashTable("){case (str, intV) => str + intV + ","}).dropRight(1) + ")"
}

object CuckooHashTable {
  val growthRatio = 1.5
  private val absMask = 0x7FFFFFFF

  def apply[Key,Value]() = new CuckooHashTable[Key, Value](Vector(null, null), 1)

  def hash1(i: Int, len: Int): Int = {
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

  def hash2(i: Int, len: Int): Int = (xorShift32(i) & absMask) % len//math.abs(baseHash(i) % backing.length)

  def sundaram(primesBelow: Int) = {
    //val sqrt is a workaround for a segfault issue in scala 2.9.1
    val n = (primesBelow - 2)/2
    val nonPrimes = for (i <- 1 to n; j <- i to (n - i) / (2 * i + 1)) yield i+j+(2*i*j)
    2 +:((1 to n) diff (nonPrimes) map (2*_+1))
  }

  val primes = sundaram(500000)

  //def divHash(i: Int, len: Int): Int = math.abs(i / divVal % len)
}

