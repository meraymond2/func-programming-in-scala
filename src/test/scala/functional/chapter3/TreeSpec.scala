package functional.chapter3

import org.scalatest._

class TreeSpec extends FlatSpec with Matchers {

  val tree: Tree[String] = Branch(
    Branch(Branch(Leaf("Sherlock"), Leaf("Luna")), Leaf("cat")),
    Leaf("Cascat")
  )

  val tree2: Tree[Int] = Branch(
    Branch(Leaf(3), Branch(Leaf(7), Leaf(2))),
    Branch(Branch(Leaf(5), Branch(Leaf(12), Leaf(9))), Leaf(4))
  )

  val lopsided: Tree[Int] = Branch(
    Leaf(0),
    Branch(
      Leaf(0), Branch(
        Leaf(0), Branch(
          Leaf(0), Branch(
            Leaf(0), Leaf(0)
          )
        )
      )
    )
  )

  "size" should "return the number of nodes" in {
    Tree.size(tree) shouldEqual 7
    Tree.size(tree2) shouldEqual 13
  }

  "maximum" should "return the greatest number in a Tree[Int]" in {
    Tree.maximum(tree2) shouldEqual 12
  }

  "depth" should "return the max depth in a tree" in {
    Tree.depth(tree) shouldEqual 4
    Tree.depth(tree2) shouldEqual 5
    Tree.depth(lopsided) shouldEqual 6
  }

  val strLength = Branch(
    Branch(Branch(Leaf(8), Leaf(4)), Leaf(3)),
    Leaf(6)
  )
  val strTree = Branch(
    Branch(Leaf("3"), Branch(Leaf("7"), Leaf("2"))),
    Branch(Branch(Leaf("5"), Branch(Leaf("12"), Leaf("9"))), Leaf("4"))
  )

  "map" should "apply a function to each element in a tree" in {
    Tree.map(tree)(_.length) shouldEqual strLength

    Tree.map(tree2)(_.toString()) shouldEqual strTree
  }

  "fold" should "iterate through a tree and return a new value" in {
    // size
    def size[A](tree: Tree[A]): Int =
      Tree.fold(tree, 0)((_, z) => z + 1)((l, r) => 1 + l + r)

    size(tree) shouldEqual 7
    size(tree2) shouldEqual 13

    // maximum
    val max = Tree.fold(_: Tree[Int], 0)((a, z) => a max z)((l, r) => l max r)
    max(tree2) shouldEqual 12

    // depth
    def depth[A](tree: Tree[A]): Int =
      Tree.fold(tree, 0)((_, z) => z + 1)((l, r) => 1 + (l max r))

    depth(tree) shouldEqual 4
    depth(tree2) shouldEqual 5
    depth(lopsided) shouldEqual 6

    // map
    def map[A,B](tree: Tree[A], z: Tree[B])(f: A => B): Tree[B] =
      Tree.fold[A,Tree[B]](tree, z)((a, _) => Leaf(f(a)))((l, r) => Branch(l, r))

    // I don’t know how to get around the Leaf(0)
    map(tree, Leaf(0))(_.length) shouldEqual strLength
    map(tree2, Leaf(""))(_.toString()) shouldEqual strTree
  }

}


