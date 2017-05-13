import patmat.Huffman._

object patmat {
  println("Welcome to the Scala worksheet")

  val sampleTree = Huffman.makeCodeTree(
    makeCodeTree(Leaf('x', 1), Leaf('e', 1)),
    Leaf('t', 2))
}