package forcomp

import Anagrams._

object AnagramWorksheet {
  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet
  //val so = sentenceOccurrences(List())
	//val combos = combinations(so)
	//legalOccurrences(combos)
	
	val yesManList = List("yes", "man")       //> yesManList  : List[java.lang.String] = List(yes, man)
	val yMSentOcc = sentenceOccurrences(yesManList)
                                                  //> yMSentOcc  : forcomp.Anagrams.Occurrences = List((a,1), (e,1), (m,1), (n,1),
                                                  //|  (s,1), (y,1))
	//val yesMan = combinations(yMSentOcc)
	//legalOccurrences(yMSentOcc)
	//val answer = sentenceAnagrams(yesManList)
	//answer
	combinations(Nil)                         //> res0: List[forcomp.Anagrams.Occurrences] = List(List())
	Nil :: Nil                                //> res1: List[scala.collection.immutable.Nil.type] = List(List())
	legalOccurrences(Nil)                     //> res2: List[forcomp.Anagrams.Sentence] = List()
	//val word = "abel"
	//val sentence = List(word)
	//val occ = sentenceOccurrences(sentence)
	//val combos = combinations(occ)
	//combos.map(comb => dictionaryByOccurrences(comb))
}