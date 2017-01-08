package sol.search.sol

import scala.xml.Node
import scala.xml.NodeSeq

import scala.io.Source
import scala.util.matching.Regex
import search.src.PorterStemmer
import scala.collection.mutable.LinkedList

import java.util.HashMap
import java.io._

/**
  * A class that represents the indexer for our search engine. The indexer parses,
  * tokenizes, and stems input to create txt files for the querier to use.
  */
class Indexer () {
  /**
    * A method that creates the hashmap storing a word that maps onto another hashmap
    * which stores the documents the word appears in as well as the number of times the
    * word appears in each document
    * 
    * @param texts - list of documents in the corpus
    * @param ids - page IDs of documents
    * @return - a hashmap storing a word that maps onto another hashmap
    * which stores the documents the word appears in as well as the number of times the
    * word appears in each document
    */
  def wordToDocMapFun(
    texts: Array[List[String]], 
    ids: Array[String]): HashMap[String, HashMap[String, Int]] = {
    var wordMap = new HashMap[String, HashMap[String, Int]]
    for(i <- 0 to texts.length - 1) {
      for(j <- 0 to texts(i).length - 1) {
        if(wordMap.containsKey(texts(i)(j))) {
           var docMap = wordMap.get(texts(i)(j))
           var wordCount = docMap.get(ids(i)) + 1
           docMap.put(ids(i), wordCount)  
           wordMap.put(texts(i)(j), docMap)
        } else {
          var docMap = new HashMap[String, Int]
          docMap.put(ids(i), 1)
          wordMap.put(texts(i)(j), docMap)
        }     
      }
    }
    wordMap
  }
  
  /**
    * Method that gets the max word count for a word in all documents in the corpus
    * 
    * @param word - word to be looked up
    * @param map - String -> Int hashmap that maps the documents the word appears in 
    * to the number of times the word appears in the document
    * @return - the max word count of the word 
    */
  def getMaxWordCount(word: String, map: HashMap[String, Int]): Int = {
    var maxWordCount: Int = 0
    var mapVals = map.values()
    val valIterator = mapVals.iterator()
    while(valIterator.hasNext()) {
      var wordCount = valIterator.next()
      if(wordCount > maxWordCount) {
        maxWordCount = wordCount
      }
    }
   maxWordCount
  }
  /**
    * Calculates the term frequency of a word in the corpus
    * 
    * @param word - the word whose frequency is being calculated
    * @param map - a hashmap that maps documents the word is contained in to the]
    * number of times the word appears in the document
    * 
    * @param - the term frequency of the word 
    */
  def termFrequency(word: String, doc: String, map: HashMap[String, Int]): Double = {
    var wordCount: Int = map.get(doc)
    wordCount.toDouble / getMaxWordCount(word, map).toDouble
  }

  /**
    * Calculates the inverse document frequency of a word
    * 
    * @param docs - an array of all the document IDs in the corpus 
    * @param map - a hash map that maps a word to a hashmap that maps 
    * the documents the word is contained in to the number of times the word 
    * appears in the document
    * @return - the inverse document frequency of the word
    */
  def inverseDocumentFrequency(
    docs: Array[String], word : String,
    map: HashMap[String, HashMap[String, Int]]): Double = {
    val n: Int = docs.length
    val ni: Int = map.get(word).size()
    Math.log(n.toDouble / ni.toDouble)
  }
  
  /**
    * Calculates the word score of a word in a document
    * 
    * @param word - the word whose score is being calculated
    * @param docID - ID of the document
    * @param docs - an array of all the document IDs in the corpus
    * @param map - a hash map that maps a word to a hashmap that maps 
    * the documents the word is contained in to the number of times the word 
    * appears in the document
    * @return - the word score
    */
  def wordScore(
    word: String, 
    docID: String, 
    docs: Array[String],
    map: HashMap[String, HashMap[String, Int]]): Double =
    termFrequency(word, docID, map.get(word)) * inverseDocumentFrequency(docs, word, map)
  
  /**
    * Creates a hashmap mapping a word to the documents it is contained in to the score
    * of the word in that particular document
    * 
    * @param texts - an array of list of strings representing each document in the corpus
    * @param ids - An array of all the document IDs in the corpus 
    * @return - a hashmap mapping a word to the documents it is contained in to the score
    * of the word in that particular document 
    */
  def wordScoreHashMapFun(
    texts : Array[List[String]], 
    ids : Array[String]) : HashMap[String, HashMap[String,Double]] = {
    var map = wordToDocMapFun(texts, ids)
    var wordMap = new HashMap[String, HashMap[String,Double]]
    for(i <- 0 to texts.length - 1) {
      for(j <- 0 to texts(i).length - 1) {
        if(!wordMap.containsKey(texts(i)(j))) {
           var scoreMap = new HashMap[String,Double]
           val idsArray = map.get(texts(i)(j)).keySet().
             toArray(new Array[String](map.get(texts(i)(j)).keySet().size()))
           for(k <- 0 to idsArray.length - 1) {
             val wordScorex = wordScore(texts(i)(j), idsArray(k), ids, map)
             val docID = idsArray(k)
             scoreMap.put(docID, wordScorex) 
           }
           wordMap.put(texts(i)(j), scoreMap)
        }     
      }
    }
    wordMap
  }
  
 /**
   * Calculates the weight of one document on another to be used for Hat Rank
   * 
   * @param n - the number of documents
   * @param pageJ - a page
   * @param pageK - a different page whose weight on pageJ is being calculated
   * @param linkMap - a hashmap mapping a document ID to all the valid 
   * links in the document  
   * @return - the weight of pageK on pageJ
   */
 def rankWeight(
   n: Int, 
   pageJ: String, 
   pageK: String, 
   linkMap: HashMap[String, List[String]]): Double = {
   if(linkMap.get(pageK).contains(pageJ)) {
     (0.15 / n.toDouble) + ((1.0 - 0.15) * (1.0 / (linkMap.get(pageK).length.toDouble))) 
   } else {
     (0.15 / n.toDouble)
   }
 }
 
 /**
   * Calculates the rank of each document and stores info in a hashmap mapping 
   * documents to hat rank scores
   * 
   * @param linkMap - a hashmap of documents to links in the document
   * @param titleMap - a hashmap of document IDs to document titles
   * @return - a hashmap mapping documents to hat rank scores
   */
 def hatRank(
   linkMap: HashMap[String, List[String]], 
    titleMap: HashMap[String, String]) : HashMap[String, Double] = {
    var rankMap = new HashMap[String, Double]
    var rankMapPrev = new HashMap[String, Double]
    val pages: Array[String] = 
      linkMap.keySet().toArray(new Array[String](linkMap.keySet().size))
    
    for(i <- 0 to pages.length - 1) {
      rankMap.put(pages(i), (1.0 / pages.length.toDouble))
    }
    
    do {
      for(j <- 0 to pages.length - 1) {
        rankMapPrev.put(pages(j), rankMap.get(pages(j)))
      }
      for(j <- 0 to pages.length - 1) {
        var rankArray = new Array[Double](pages.length)
        for(k <- 0 to pages.length - 1) {
          rankArray(k) = 
            (rankWeight(pages.length, titleMap.get(pages(j)), pages(k), linkMap) * 
              rankMapPrev.get(pages(k)))
        }
        var rankJ = rankArray.sum
        rankMap.put(pages(j), rankJ)
      }
    } while(
        Math.sqrt(pages.
        map(p => rankMap.get(p) - rankMapPrev.get(p)).
        map(p => p*p).sum) > 0.00001)
      
    rankMap
  }
}


object Indexer { 
  
  def main(args: Array[String]): Unit = {
    val Index = new Indexer()
 
    val titles: Array[String] = (
      xml.XML.loadFile(args(0)) \ "page" \ "title").toString().
      split("\n").filterNot((s: String) => s.equals("<title>")).
      filterNot((s: String) => s.equals("</title><title>")).
      filterNot((s: String) => s.equals("</title>"))
      
    val texts: Array[String] = (xml.XML.loadFile(args(0)) \ "page" \ "text").toString().
      split("</text>").filterNot((s: String) => s.equals("<text>")).
      filterNot((s: String) => s.equals("</text><text>")).
      filterNot((s: String) => s.equals("</text>")).map {x => x.drop(6)}
                            
    val ids: Array[String] = (
      xml.XML.loadFile(args(0)) \ "page" \ "id").toString().
      split("\n").filterNot((s: String) => s.equals("<id>")).
      filterNot((s: String) => s.equals("</id><id>")).
      filterNot((s: String) => s.equals("</id>"))
  
   val idTitleHashMap: HashMap[String, String] = {
      val x = new HashMap[String, String]
      (ids.zip(titles)).map {t => x.put(t._1, t._2)}
      x
   } 
   
   val textRegex = new Regex("""[^\W_]+’[^\W_]+|[^\W_]+""")
   
   val linkRegex = new Regex("""\[\[[^\[]+?\]\]""")
   
   val tokenizedLink = new Regex("""^.*(?=(\|))|\A[^|]*\z""")
   
   val links: Array[List[String]] = texts.
     map {t => (linkRegex.findAllMatchIn(t).
     toList.map {aMatch => aMatch.matched}).
     map {link => link.drop(2).dropRight(2)}.
     flatMap(x => tokenizedLink.findAllMatchIn(x).
     toList.map {aMatch => aMatch.matched})}
       
   val tokenizedTexts: Array[List[String]] = texts.
     map{t => textRegex.findAllMatchIn(t).toList.map {aMatch => aMatch.matched}}.
     map {tList => tList.map {tItem => PorterStemmer.stem(tItem.toLowerCase())}}
                                         
   val idLinkHashMap: HashMap[String, List[String]] = {
     val x = new HashMap[String, List[String]]
     for ((k, v) <- (
       ids.zip(links))) {
       var validLinks = List[String]()
       for(link <- v) {
         if(titles.contains(link) && idTitleHashMap.get(k) != link) {
           validLinks = link::validLinks
         }
       }
       x.put(k, validLinks)
     }
     x
   }
   
   val wordScoreHashMap: HashMap[String, HashMap[String, Double]] = 
     Index.wordScoreHashMapFun(
       (((xml.XML.loadFile(args(0)) \ "page" \ "text").toString().
       split("</text>").filterNot((s: String) => s.equals("<text>")).
       filterNot((s: String) => s.equals("</text><text>")).
       filterNot((s: String) => s.equals("</text>"))).map {x => x.drop(6)}).
       map{t => textRegex.findAllMatchIn(t).toList.map {aMatch => aMatch.matched}}.
       map {tList => tList.map {tItem => PorterStemmer.stem(tItem.toLowerCase())}}, 
       ids)
     
   val hatRankMap: HashMap[String, Double] = Index.hatRank(idLinkHashMap, idTitleHashMap)
  
   val idTitleWriter = new BufferedWriter(new FileWriter(args(1)))
   val hatRankWriter = new BufferedWriter(new FileWriter(args(2)))
   val wordScoreWriter = new BufferedWriter(new FileWriter(args(3)))
   
   idTitleHashMap.keySet().toArray().
     foreach(k => idTitleWriter.write(k + " " + idTitleHashMap.get(k) + "\n"))
   hatRankMap.keySet().toArray().
     foreach(k => hatRankWriter.write(k + " " + hatRankMap.get(k) + "\n"))
   wordScoreHashMap.keySet().toArray().
     foreach(k => wordScoreHashMap.get(k).keySet().toArray().foreach(id => 
   wordScoreWriter.write(k + " " + id + " " + wordScoreHashMap.get(k).get(id) + "\n")))
          
   idTitleWriter.close
   hatRankWriter.close
   wordScoreWriter.close
   
   
  }
}