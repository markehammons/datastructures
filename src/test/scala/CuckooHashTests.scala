/**
 * Created with IntelliJ IDEA.
 * User: luft
 * Date: 1/29/14
 * Time: 1:34 PM
 */

import org.scalacheck.Properties
import org.scalacheck.Prop.forAll
import org.scalacheck.Prop.forAllNoShrink
import org.scalacheck.Arbitrary.arbitrary


object CuckooHashTests extends Properties("CuckooHashTable") {

  val maxAvgLoad = 1.7

  def getDistribution(xs: Traversable[Int]): Map[Int,Int] = (xs foldLeft Map[Int,Int]().withDefault(_ => 0)) {
    case (map, x) => map + (x -> (map(x) + 1))
  }

  def within(x: Int, percent: Double) = x + (x*percent) >= x && x - (x*percent) <= x

  property("growth") = forAll { (l: List[Int]) =>
    val table = (l foldLeft CuckooHashTable[Int, Null]()) {case (table, str) => table.insert(str, null)}

    l.forall(s => table.backing1.contains((s,null)) || table.backing2.contains((s,null)))
  }

  property("retrieval") = forAll{ (ks: Set[Int], vs: Set[Int]) =>
    val v = ks zip vs
    val table = (v foldLeft CuckooHashTable[Int, Int]()) {case (table, (int,str)) => table.insert(int, str)}

    v forall {case (key, value) => {if(table.get(key) != value) println(s"${table.get(key)} != $value"); table.get(key) == value}}
  }

  /*property(">25% load") = forAll{ (ks: Set[Int], vs: Set[Int]) =>
    val v = ks zip vs
    val table = (v foldLeft CuckooHashTable[Int, Int]()) {case (table, (int,str)) => table.insert(int, str)}

    println("----------------------DONE-------------------------")

    v.size / table.backing.size.toDouble >= .25 || v.size == 0
  }*/

  /*property("hash1 distribution near normal?") = forAllNoShrink(arbitrary[Set[Int]] suchThat (_.size > 25))((xs) => {
    val len = xs.size
    val hashDistrib = getDistribution(xs.toList map (CuckooHashTable.hash1(_,len)))

    val mean = ((hashDistrib.toList map (_._2)).sum / hashDistrib.size.toDouble)
      /*val median = (hashDistrib.toVector map (_._2)).sorted.apply(hashDistrib.size / 2)
      val mode = ((getDistribution(hashDistrib.toList map (_._2)) foldLeft (0,0)) { case ((modeVal, mTimes), (dVal, times)) => if(times > mTimes) (dVal, times) else (modeVal, mTimes)})._1

      val avg = (mean + median + mode) / 3.0


      println(s"len: $len")
      println(s"mean: $mean\n median: $median\n mode:$mode")
      println(hashDistrib)
      println(s"mean distribution: $avg")*/

    mean < maxAvgLoad
  })

  property("hash2 distribution near normal?") = forAllNoShrink(arbitrary[Set[Int]] suchThat (_.size > 25))((xs) => {
    val len = xs.size
    val hashDistrib = getDistribution(xs.toList map (CuckooHashTable.hash2(_,len)))

    val mean = (hashDistrib.toList map (_._2)).sum / hashDistrib.size.toDouble
      //val median = (hashDistrib.toVector map (_._2)).sorted.apply(hashDistrib.size / 2)
      //val mode = ((getDistribution(hashDistrib.toList map (_._2)) foldLeft (0,0)) { case ((modeVal, mTimes), (dVal, times)) => if(times > mTimes) (dVal, times) else (modeVal, mTimes)})._1

      //val avg = (mean + median + mode) / 3.0


      //println(s"len: $len")
      //println(s"mean: $mean\n median: $median\n mode:$mode")
      //println(hashDistrib)
      //println(s"mean distribution: $avg")

    mean < maxAvgLoad
  })*/

  property("HashTable load always > 30%") = forAll(arbitrary[List[Int]] suchThat (_.size > 1)) ((xs) => {
    val table = (xs foldRight CuckooHashTable[Int, Null]) {
      case (int, table) => table.insert(int, null)
    }

    table.size.toDouble / table.reserved * 2 > .3
  })
}
