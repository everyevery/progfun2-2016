import org.scalacheck._
import org.scalacheck.Gen._
import org.scalacheck.Prop.{forAll, BooleanOperators}
import Arbitrary.arbitrary

val propConcatLists = forAll {
  (l1: List[Int], l2: List[Int]) =>
    l1.size + l2.size == (l1 ::: l2).size
}
propConcatLists.check

val propSqrt = forAll { (n: Int) =>
scala.math.sqrt(n*n) == n}
propSqrt.check

val propReverse = forAll {l: List[String] => l.reverse.reverse == l }
propReverse.check

val smallInteger = Gen.choose(0, 100)
val propSmallInteger = forAll(smallInteger) {n: Int =>
  0 <= n && n <= 100
}
propSmallInteger.check

val propMakeList = forAll { n: Int =>
  (n >= 0 && n < 1000) ==> (List.fill(n)("").length == n)
}

propMakeList.check

lazy val genMap: Gen[Map[Int,Int]] = for {
  k <- arbitrary[Int]
  v <- arbitrary[Int]
  m <- oneOf(const(Map.empty[Int,Int]), genMap)
} yield m.updated(k, v)

def matrix[T](g: Gen[T]): Gen[Seq[Seq[T]]] = Gen.sized { size =>
  val side = scala.math.sqrt(size).asInstanceOf[Int]
  Gen.listOfN(side, Gen.listOfN(side, g))
}

val smallEvenInteger = Gen.choose(0,200) suchThat (_ % 2 == 0)

