package forcomp

import Anagrams._

object AnagramWorksheet {;import org.scalaide.worksheet.runtime.library.WorksheetSupport._; def main(args: Array[String])=$execute{;$skip(105); 
  println("Welcome to the Scala worksheet");$skip(141); 
  //val so = sentenceOccurrences(List())
	//val combos = combinations(so)
	//legalOccurrences(combos)
	
	val yesManList = List("yes", "man");System.out.println("""yesManList  : List[java.lang.String] = """ + $show(yesManList ));$skip(49); 
	val yMSentOcc = sentenceOccurrences(yesManList);System.out.println("""yMSentOcc  : forcomp.Anagrams.Occurrences = """ + $show(yMSentOcc ));$skip(145); val res$0 = 
	//val yesMan = combinations(yMSentOcc)
	//legalOccurrences(yMSentOcc)
	//val answer = sentenceAnagrams(yesManList)
	//answer
	combinations(Nil);System.out.println("""res0: List[forcomp.Anagrams.Occurrences] = """ + $show(res$0));$skip(12); val res$1 = 
	Nil :: Nil;System.out.println("""res1: List[scala.collection.immutable.Nil.type] = """ + $show(res$1));$skip(23); val res$2 = 
	legalOccurrences(Nil);System.out.println("""res2: List[forcomp.Anagrams.Sentence] = """ + $show(res$2))}
	//val word = "abel"
	//val sentence = List(word)
	//val occ = sentenceOccurrences(sentence)
	//val combos = combinations(occ)
	//combos.map(comb => dictionaryByOccurrences(comb))
}