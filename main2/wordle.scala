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


def pool(secret: String, word: String) : List[Char] = ??? 

def aux(secret: List[Char], word: List[Char], pool: List[Char]) : List[Tip] = ???

def score(secret: String, word: String) : List[Tip] = ???


// score("chess", "caves") // => List(Correct, Absent, Absent, Present, Correct)
// score("doses", "slide") // => List(Present, Absent, Absent, Present, Present)
// score("chess", "swiss") // => List(Absent, Absent, Absent, Correct, Correct)
// score("chess", "eexss") // => List(Present, Absent, Absent, Correct, Correct)

// (4)
def eval(t: Tip) : Int = ???

def iscore(secret: String, word: String) : Int = ???

//iscore("chess", "caves") // => 21
//iscore("chess", "swiss") // => 20

// (5)
def lowest(secrets: List[String], word: String, current: Int, acc: List[String]) : List[String] = ???

def evil(secrets: List[String], word: String) : List[String] = ???


//evil(secrets, "stent").length
//evil(secrets, "hexes").length
//evil(secrets, "horse").length
//evil(secrets, "hoise").length
//evil(secrets, "house").length

// (6)
def frequencies(secrets: List[String]) : Map[Char, Double] = ???

// (7)
def rank(frqs: Map[Char, Double], s: String) : Double = ???

def ranked_evil(secrets: List[String], word: String) : List[String] = ???


}
