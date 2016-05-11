import Huffman._

object Huffman {import scala.runtime.WorksheetSupport._; def main(args: Array[String])=$execute{;$skip(78); 
  println("Welcome to the Scala worksheet");$skip(92); 
val sampleTree = makeCodeTree(
  makeCodeTree(Leaf('x', 1), Leaf('e', 1)),
  Leaf('t', 2)
);System.out.println("""sampleTree  : <error> = """ + $show(sampleTree ))}

}