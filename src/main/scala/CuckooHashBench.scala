import scala.collection.immutable.HashMap

/**
 * Created with IntelliJ IDEA.
 * User: luft
 * Date: 1/29/14
 * Time: 2:28 PM
 */
object CuckooHashBench extends App {

  def sleep(ms: Long) = {
    val start = System.currentTimeMillis()
    while(System.currentTimeMillis() - start < ms) {;}
  }

  def bench(code: => Unit) = {
    val start = System.nanoTime()
    code
    System.nanoTime() - start
  }

  var x = HashMap[Int,Int]()
  var y = CuckooHashTable[Int,Int]

  //sleep(5000)

  val numKeys = 50000


  def randomInts: Stream[Int] = scala.util.Random.nextInt() #:: randomInts
  val ks = randomInts.take(numKeys)
  val vs = randomInts.take(numKeys)
  val both = ks zip vs toList

  //val bothS = sequentialInts.take(numKeys) zip randomInts.take(numKeys) toList

  def sequentialInts: Stream[Int] = 1 #:: sequentialInts.map(_+1)

  val xtimes = for(i <- 0 to 100) yield bench(
    x = (both foldLeft x) {case (x,(k,v)) => {x.+((k,v))}}
  )

  println(xtimes.sum / 100.0)

  println(x.size)

  var z = CuckooHashTable[Int,Int]
  /*println(bench(
    y = (both foldLeft y) {case (t,(k,v)) => t.insert(k,v)}
  ))*/

  CuckooHashTable.primes
  val times = for(i <- 0 to 100) yield {
    bench(z = (both foldLeft z) {case (t,(k,v)) => t.insert(k,v)})
  }

  println(times.sum / 100.0)


  println(bench(both map (kv => x.get(kv._1))))
  println(bench(both.map(kv => z.get(kv._1))))



  //println(y.backing.size)
  //println(numKeys.toDouble / y.backing.size)

  //println(z)
  println(z.backing.size)
  println(numKeys.toDouble / z.backing.size)
}
