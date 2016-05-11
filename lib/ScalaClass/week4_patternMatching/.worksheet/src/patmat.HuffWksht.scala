package patmat

import patmat.Huffman._

object HuffWksht {import scala.runtime.WorksheetSupport._; def main(args: Array[String])=$execute{;$skip(103); 
  println("Welcome to the Scala worksheet");$skip(98); 

  val sampleTree = makeCodeTree(
    makeCodeTree(Leaf('x', 1), Leaf('e', 1)),
    Leaf('t', 2));System.out.println("""sampleTree  : patmat.Huffman.Fork = """ + $show(sampleTree ));$skip(113); 
    
	//chars(sampleTree)
  //weight(sampleTree)
  
  val letters = List('a', 'b', 'a', 'c', 'd', 'd', 'b', 'b');System.out.println("""letters  : List[Char] = """ + $show(letters ));$skip(33); 
  val letterMap = times(letters);System.out.println("""letterMap  : List[(Char, Int)] = """ + $show(letterMap ));$skip(36); val res$0 = 
  letterMap.sortBy(pair => pair._2);System.out.println("""res0: List[(Char, Int)] = """ + $show(res$0));$skip(206); val res$1 = 
  //times2(List('a', 'b', 'a', 'c', 'd', 'b'))
  //times3(List('a', 'b', 'a', 'c', 'd', 'b'))
  //val sll = makeOrderedLeafList(letterMap)
	//singleton(sll)
	//singleton(Nil)
	singleton(Leaf('a',1) :: Nil);System.out.println("""res1: Boolean = """ + $show(res$1));$skip(46); val res$2 = 
	singleton(Leaf('a',1) :: Leaf('a',1) :: Nil);System.out.println("""res2: Boolean = """ + $show(res$2));$skip(37); 
  val tree = createCodeTree(letters);System.out.println("""tree  : patmat.Huffman.CodeTree = """ + $show(tree ));$skip(25); val res$3 = 
  decodedSecret.toString;System.out.println("""res3: String = """ + $show(res$3));$skip(56); val res$4 = 
  decode(frenchCode, encode(frenchCode)(decodedSecret));System.out.println("""res4: List[Char] = """ + $show(res$4));$skip(50); 
  val qe = quickEncode(frenchCode)(decodedSecret);System.out.println("""qe  : List[patmat.Huffman.Bit] = """ + $show(qe ));$skip(25); val res$5 = 
  decode(frenchCode, qe);System.out.println("""res5: List[Char] = """ + $show(res$5));$skip(25); val res$6 = 
	createCodeTree(letters);System.out.println("""res6: patmat.Huffman.CodeTree = """ + $show(res$6));$skip(38); val res$7 = 
  makeOrderedLeafList(times(letters));System.out.println("""res7: List[patmat.Huffman.Leaf] = """ + $show(res$7));$skip(195); 
                                                  
  def flatten(xs: List[Any]): List[Any] = xs match {
		case Nil => Nil
		case (y :: ys) :: tail => y :: flatten(ys ::: tail)
		case _ => xs
  };System.out.println("""flatten: (xs: List[Any])List[Any]""");$skip(39); val res$8 = 
	flatten(List(1,2) :: 5 :: List(3, 4));System.out.println("""res8: List[Any] = """ + $show(res$8));$skip(7); val res$9 = 
	3 / 2;System.out.println("""res9: Int(1) = """ + $show(res$9))}
}