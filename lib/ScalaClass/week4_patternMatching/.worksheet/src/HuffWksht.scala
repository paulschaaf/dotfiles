import Huffman._

object HuffWksht {import scala.runtime.WorksheetSupport._; def main(args: Array[String])=$execute{;$skip(80); 
  println("Welcome to the Scala worksheet");$skip(98); 

  val sampleTree = makeCodeTree(
    makeCodeTree(Leaf('x', 1), Leaf('e', 1)),
    Leaf('t', 2));System.out.println("""sampleTree  : <error> = """ + $show(sampleTree ))}
}