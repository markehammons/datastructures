import scala.collection.immutable.HashMap
import scala.io.Source

/**
 * Created with IntelliJ IDEA.
 * User: luft
 * Date: 1/29/14
 * Time: 2:28 PM
 */
object CuckooHashBench {
  implicit class NanoSeconds(val l: Long) extends AnyVal {
    def toMilliseconds = l / 100000
  }


  def bench(code: => Unit) = {
    val start = System.nanoTime()
    code
    System.nanoTime() - start
  }

  def main(args: Array[String]) = {
    println(s"How long does it take to calculate the primes under 2^31? ${bench(CuckooHashTable.growthRatio).toMilliseconds}ms")


    var x = HashMap[Int,Int]()
    var y = CuckooHashTable[Int,Int]

    //sleep(5000)

    val numKeys = if(args.length >= 1)
      args(0).toInt
    else
      5000

    println(s"Benching how long it takes to insert $numKeys keys into our hashtables.")


    def randomInts: Stream[Int] = scala.util.Random.nextInt() #:: randomInts
    val ks = randomInts.take(numKeys)
    val vs = randomInts.take(numKeys)
    val both = ks zip vs toList

    //val bothS = sequentialInts.take(numKeys) zip randomInts.take(numKeys) toList

    def sequentialInts: Stream[Int] = 1 #:: sequentialInts.map(_+1)

    /*val xtimes = for(i <- 0 to 100) yield bench(
      x = (both foldLeft x) {case (x,(k,v)) => {x.+((k,v))}}
    )

    println(xtimes.sum.toMilliseconds / 100.0)

    println(x.size)*/

    //sleep(25000)


    var z = CuckooHashTable[Int,Int]

    /*println(bench(
      y = (both foldLeft y) {case (t,(k,v)) => t.insert(k,v)}
    ))*/

    //CuckooHashTable.primes

    /*for(i <- 0 to 5) {
      (both foldLeft z) {case (t,(k,v)) => t.insert(k,v)}
    }*/

    val times = for(i <- 0 to 100) yield {
      bench(z = (both foldLeft z) {case (t,(k,v)) => t.insert(k,v)})
    }

    println(times.sum.toMilliseconds / 100.0)


    //print(s"Scala Hashgetbench:${bench(both map (kv => x.get(kv._1)))}\n me:")
    //println(bench(both.map(kv => z.get(kv._1))))



    //println(y.backing.size)
    //println(numKeys.toDouble / y.backing.size)

    //println(z)
    println(z.size)
    println(z.reserved*2)
    println(z.size.toDouble*100 / (z.reserved*2))

    println("press enter to continue...")

    val w = System.in.read()
  }
}
