import Huffman._

object Huffman {
  println("Welcome to the Scala worksheet")
val sampleTree = makeCodeTree(
  makeCodeTree(Leaf('x', 1), Leaf('e', 1)),
  Leaf('t', 2)
)

}