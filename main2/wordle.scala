// Main Part 2 about Evil Wordle
//===============================


object M2 { 

import io.Source
import scala.util._

// ADD YOUR CODE BELOW
//======================


//(1)

def split(s: String) : List[String] = s.split("\n").toList

def get_wordle_list(url: String) : List[String] = {
    Try ( split(Source.fromURL(url) ("ISO-8859-1").mkString) ).getOrElse(Nil)
}

// val secrets = M2.get_wordle_list("https://nms.kcl.ac.uk/christian.urban/wordle.txt")
// secrets.length // => 12972
// secrets.filter(_.length != 5) // => Nil

//(2)
def removeN[A](xs: List[A], elem: A, n: Int) : List[A] = 
    xs match {
        case Nil => Nil

        case hd::tl if (hd::tl).count(_ == elem) < n => xs.filter(_ != elem)

        case hd::tl if (hd::tl).count(_ == elem) >= n && n == 0 => (hd::tl)

        case hd::tl if (hd::tl).count(_ == elem) >= n && n > 0 => {
            if(hd != elem){
                hd :: removeN(tl, elem, n)
            }
            else {
                removeN(tl, elem, n-1)
            }
        }
        

        case _ => Nil
    }



// M2.removeN(List(1,2,3,2,1), 3, 1)  // => List(1, 2, 2, 1)
// M2.removeN(List(1,2,3,2,1), 2, 1)  // => List(1, 3, 2, 1)
// M2.removeN(List(1,2,3,2,1), 1, 1)  // => List(2, 3, 2, 1)
// M2.removeN(List(1,2,3,2,1), 0, 2)  // => List(1, 2, 3, 2, 1)

// (3)
abstract class Tip
case object Absent extends Tip
case object Present extends Tip
case object Correct extends Tip


def pool(secret: String, word: String) : List[Char] = 
    (secret.toList, word.toList) match {
        case (Nil, _) => Nil

        case (secFirst :: secRest, wordFirst :: wordRest) if secFirst == wordFirst => pool(secRest.mkString, wordRest.mkString)

        case(secFirst :: secRest, wordFirst :: wordRest) if secFirst != wordFirst => secFirst :: pool(secRest.mkString, wordRest.mkString)

        case (_, _) => Nil
    }

def aux(secret: List[Char], word: List[Char], pool: List[Char]) : List[Tip] = 
    (secret, word) match {

        case (Nil, _) => Nil

        case (secFirst :: secRest, wordFirst :: wordRest) if secFirst == wordFirst => Correct :: aux(secRest, wordRest, pool)

        case (secFirst :: secRest, wordFirst :: wordRest) if (secFirst != wordFirst && pool.contains(wordFirst)) => {
            val poolLetterRemoved = removeLetterOccurrence(wordFirst, pool)

            Present :: aux(secRest, wordRest, poolLetterRemoved)
        }
        case (secFirst :: secRest, wordFirst :: wordRest) if (secFirst != wordFirst && !pool.contains(wordFirst)) => Absent :: aux(secRest, wordRest, pool)

        case (_, _) => Nil

    }

def removeLetterOccurrence(wordFirst: Char, pool: List[Char]) : List[Char] = 

    pool match {
        case Nil => Nil
        case x::xs if x == wordFirst => xs
        case x::xs if x != wordFirst => x :: removeLetterOccurrence(wordFirst, xs)
        case _ => Nil
    }

def score(secret: String, word: String) : List[Tip] = aux(secret.toList, word.toList, pool(secret, word))


// score("chess", "caves") // => List(Correct, Absent, Absent, Present, Correct)
// score("doses", "slide") // => List(Present, Absent, Absent, Present, Present)
// score("chess", "swiss") // => List(Absent, Absent, Absent, Correct, Correct)
// score("chess", "eexss") // => List(Present, Absent, Absent, Correct, Correct)

// (4)
def eval(t: Tip) : Int = 
    t match {
        case Correct => 10

        case Present => 1

        case Absent => 0

        case _ => - 1
    } 

def iscore(secret: String, word: String) : Int = {
    score(secret, word) match {
        case Nil => 0

        case x::xs => (x::xs).map(eval).sum

        case null => 0
    }
}

//iscore("chess", "caves") // => 21
//iscore("chess", "swiss") // => 20

// (5)
def lowest(secrets: List[String], word: String, current: Int, acc: List[String]) : List[String] = 
    secrets match {
        case Nil => acc

        case x::xs if iscore(x, word) == current => lowest(xs, word, current, x::acc)

        case x::xs if iscore(x, word) < current => lowest(xs, word, iscore(x, word), List(x))

        case x::xs if iscore(x, word) > current => lowest(xs, word, current, acc) 

        case _ => Nil

    }



def evil(secrets: List[String], word: String) : List[String] = lowest(secrets, word, Int.MaxValue, Nil)


// M2.evil(secrets, "stent").length
//evil(secrets, "hexes").length
//evil(secrets, "horse").length
//evil(secrets, "hoise").length
//evil(secrets, "house").length

// (6)
def frequencies(secrets: List[String]) : Map[Char, Double] = {
    val letters = secrets.mkString.toList.filter(_.isLower)

    val letterGroups = letters.groupBy(identity)

    letterGroups.map { group =>
        group._1 -> (1.0 - group._2.length.toDouble / letters.length)
    }

}


// (7)
def rank(frqs: Map[Char, Double], s: String) : Double = getFreqSum(frqs, s.toList)

def getFreqSum(frqs: Map[Char, Double], s: List[Char], vals: List[Double] = Nil) : Double =
    s match {
        case Nil => vals.head

        case x::xs if frqs.keys.toList.contains(x) && vals != Nil => getFreqSum(frqs, xs,frqs.getOrElse(x, 0.0) + vals.head :: vals)

        case x::xs if frqs.keys.toList.contains(x) && vals == Nil => getFreqSum(frqs, xs, frqs.getOrElse(x, 0.0) :: vals)

        case _ => 0.0
    }


def ranked_evil(secrets: List[String], word: String) : List[String] = {
    val evilOutput = evil(secrets, word)
    val freqs =  frequencies(secrets)

    highest(evilOutput, freqs)
}


def highest(secrets: List[String],  freqs: Map[Char, Double], current: Double = Double.MinValue, acc: List[String] = Nil) : List[String] = {

    val rankedSecrets = secrets.groupBy(rank(freqs, _))
    val maxRank = rankedSecrets.keys.max

    rankedSecrets.getOrElse(maxRank, Nil)

}
}
