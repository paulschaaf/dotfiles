import patmat.Huffman._

object patmat {import scala.runtime.WorksheetSupport._; def main(args: Array[String])=$execute{;$skip(84); val res$0 = 
  println("Welcome to the Scala worksheet");System.out.println("""res0: <error> = """ + $show(res$0));$skip(106); 

  val sampleTree = Huffman.makeCodeTree(
    makeCodeTree(Leaf('x', 1), Leaf('e', 1)),
    Leaf('t', 2));System.out.println("""sampleTree  : <error> = """ + $show(sampleTree ))}
}