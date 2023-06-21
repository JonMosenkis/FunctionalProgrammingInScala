package Chapter3

import Chapter3.Tree.Branch

import scala.annotation.tailrec

enum Tree[+A] {
  case Leaf(value: A)
  case Branch(left: Tree[A], right: Tree[A])

  def size: Int = fold(_ => 1, (l, r) => 1 + l.size + r.size)

  final def leftMostValue(): A = fold(a => a, (l, r) => l.leftMostValue())

  final def depth(): Int = fold(_ => 1, (l, r) => 1 + l.depth().max(r.depth()))

  final def map[B](f: A => B): Tree[B] = this.fold(a => Leaf(f(a)), (l, r) => Branch(l.map(f), r.map(f)))

  final def fold[B](lf: A => B, f: (Tree[A], Tree[A]) => B): B = this match
    case Leaf(value) => lf(value)
    case Branch(left, right) => f(left, right)

}

object Tree {
  extension (tree: Tree[Int]) def maximum(): Int =
    def traverse(subTree: Tree[Int], max: Int): Int = subTree.fold(max.max(_), (l, r) => max.max(traverse(l, max)).max(traverse(r, max)))
    traverse(tree, tree.leftMostValue())
}