package variational.test

import variational._

import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers

class Tests extends FunSuite with ShouldMatchers {
    test("mapping over constant values") {
        V(21).map(_ + 21) should equal (V(42))
    }

    test("mapping over variable values") {
        Choice(0, V(1), V(2)).map(_ + 1) should equal (Choice(0, V(2), V(3)))
    }

    test("mapping over variable values -- merging nodes") {
        Choice(0, V(1), V(2)).map(_ * 0) should equal (V(0))
    }

    test("selecting a variable not mentioned (1)") {
        val v = Choice(1, V(1), V(2))
        v.select(0) should equal (v)
    }

    test("selecting a variable not mentioned (2)") {
        val v = Choice(1, V(1), V(2))
        v.select(2) should equal (v)
    }

    test("selecting a mentioned variable") {
        val v = Choice(1, V(1), V(2))
        v.select(1) should equal (V(1))
    }

    test("selecting a mentioned variable -- merging") {
        val v = Choice(1, Choice(2, V(1), V(2)), V(1))
        v.select(2) should equal (V(1))
    }

    test("deselecting a variable not mentioned (1)") {
        val v = Choice(1, V(1), V(2))
        v.deselect(0) should equal (v)
    }

    test("deselecting a variable not mentioned (2)") {
        val v = Choice(1, V(1), V(2))
        v.deselect(2) should equal (v)
    }

    test("deselecting a mentioned variable") {
        val v = Choice(1, V(1), V(2))
        v.deselect(1) should equal (V(2))
    }

    test("deselecting a mentioned variable -- merging") {
        val v = Choice(1, Choice(2, V(2), V(1)), V(1))
        v.deselect(2) should equal (V(1))
    }

    test("variability-aware application (1)") {
        val succ = (n : Int) => n + 1
        val twice = (n : Int) => n * 2
        val f = Choice(0, V(succ), V(twice))
        val a = Choice(0, V(5), V(10))
        f(a) should equal (Choice(0, V(6), V(20)))
    }

    test("variability-aware application (2)") {
        val succ = (n : Int) => n + 1
        val twice = (n : Int) => n * 2
        val f = Choice(1, V(succ), V(twice))
        val a = Choice(0, V(1), V(5))
        f(a) should equal (Choice(0, V(2), Choice(1, V(6), V(10))))
    }

    test("decomposition") {
        val v = Choice(1, Choice(2, V("foo"), V("bar")), Choice(3, V("foo"), V("bar")))
        v.decompose should equal (Choice(1, Choice(2, V(0), V(1)), Choice(3, V(0), V(1))), Seq("foo", "bar"))
    }

    test("indexing") {
        val indices = Choice(1, V(0), V(1))
        val common = Choice(2, V("baz"), V("boo"))
        val values = Seq(Choice(0, V("foo"), V("bar")), common)
        V.index(values, indices) should equal (Choice(0, Choice(1, V("foo"), common), Choice(1, V("bar"), common)))
    }

    test("flatMap") {
        val v = Choice(0, V('a'), Choice(2, V('b'), V('B')))

        val result = for {
          letter <- v
          normalized <- Choice(1, V(letter.toLower), V(letter.toUpper))
        } yield normalized

        result should equal (Choice(0, Choice(1, V('a'), V('A')), Choice(1, V('b'), V('B'))))
    }
}