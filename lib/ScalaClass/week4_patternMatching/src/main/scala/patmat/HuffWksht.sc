package patmat

import patmat.Huffman._

object HuffWksht {
  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet

  val sampleTree = makeCodeTree(
    makeCodeTree(Leaf('x', 1), Leaf('e', 1)),
    Leaf('t', 2))                                 //> sampleTree  : patmat.Huffman.Fork = Fork(Fork(Leaf(x,1),Leaf(e,1),List(x, e)
                                                  //| ,2),Leaf(t,2),List(x, e, t),4)
    
	//chars(sampleTree)
  //weight(sampleTree)
  
  val letters = List('a', 'b', 'a', 'c', 'd', 'd', 'b', 'b')
                                                  //> letters  : List[Char] = List(a, b, a, c, d, d, b, b)
  val letterMap = times(letters)                  //> letterMap  : List[(Char, Int)] = List((b,3), (d,2), (c,1), (a,2))
  letterMap.sortBy(pair => pair._2)               //> res0: List[(Char, Int)] = List((c,1), (d,2), (a,2), (b,3))
  //times2(List('a', 'b', 'a', 'c', 'd', 'b'))
  //times3(List('a', 'b', 'a', 'c', 'd', 'b'))
  //val sll = makeOrderedLeafList(letterMap)
	//singleton(sll)
	//singleton(Nil)
	singleton(Leaf('a',1) :: Nil)             //> res1: Boolean = true
	singleton(Leaf('a',1) :: Leaf('a',1) :: Nil)
                                                  //> res2: Boolean = false
  val tree = createCodeTree(letters)              //> tree  : patmat.Huffman.CodeTree = Fork(Leaf(b,3),Fork(Leaf(a,2),Fork(Leaf(c,
                                                  //| 1),Leaf(d,2),List(c, d),3),List(a, c, d),5),List(b, a, c, d),8)
  decodedSecret.toString                          //> res3: String = List(h, u, f, f, m, a, n, e, s, t, c, o, o, l)
  decode(frenchCode, encode(frenchCode)(decodedSecret))
                                                  //> res4: List[Char] = List(h, u, f, f, m, a, n, e, s, t, c, o, o, l)
  val qe = quickEncode(frenchCode)(decodedSecret) //> qe  : List[patmat.Huffman.Bit] = List(0, 0, 1, 1, 1, 0, 1, 0, 1, 1, 1, 0, 0,
                                                  //|  1, 1, 0, 1, 0, 0, 1, 1, 0, 1, 0, 1, 1, 0, 0, 1, 1, 1, 1, 1, 0, 1, 0, 1, 1, 
                                                  //| 0, 0, 0, 0, 1, 0, 1, 1, 1, 0, 0, 1, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 1, 0, 1)
  decode(frenchCode, qe)                          //> res5: List[Char] = List(h, u, f, f, m, a, n, e, s, t, c, o, o, l)
	createCodeTree(letters)                   //> res6: patmat.Huffman.CodeTree = Fork(Leaf(b,3),Fork(Leaf(a,2),Fork(Leaf(c,1)
                                                  //| ,Leaf(d,2),List(c, d),3),List(a, c, d),5),List(b, a, c, d),8)
  makeOrderedLeafList(times(letters))             //> res7: List[patmat.Huffman.Leaf] = List(Leaf(c,1), Leaf(d,2), Leaf(a,2), Leaf
                                                  //| (b,3))
                                                  
  def flatten(xs: List[Any]): List[Any] = xs match {
		case Nil => Nil
		case (y :: ys) :: tail => y :: flatten(ys ::: tail)
		case _ => xs
  }                                               //> flatten: (xs: List[Any])List[Any]
	flatten(List(1,2) :: 5 :: List(3, 4))     //> res8: List[Any] = List(1, 2, 5, 3, 4)
	3 / 2                                     //> res9: Int(1) = 1
}