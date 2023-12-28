// Main Part 5 about a "Compiler" for the Brainf*** language
//============================================================


object M5b {

// !!! Copy any function you need from file bf.scala !!!
//
// If you need any auxiliary function, feel free to 
// implement it, but do not make any changes to the
// templates below.


// DEBUGGING INFORMATION FOR COMPILERS!!!
//
// Compiler, even real ones, are fiendishly difficult to get
// to produce correct code. One way to debug them is to run
// example programs ``unoptimised''; and then optimised. Does
// the optimised version still produce the same result?


// for timing purposes
def time_needed[T](n: Int, code: => T) = {
  val start = System.nanoTime()
  for (i <- 0 until n) code
  val end = System.nanoTime()
  (end - start)/(n * 1.0e9)
}


type Mem = Map[Int, Int]

import io.Source
import scala.util._

// ADD YOUR CODE BELOW
//======================

// (6) 
def jtable(pg: String) : Map[Int, Int] = {
  val pgLst = pg.toList

  val indexes = getIndexList(pgLst)

  val openingBracketIndexes = indexes.filter(index => pgLst(index) == '[')

  val openingBracketPositions = openingBracketIndexes.map(index => index+1)

  val closingBracketIndexes = indexes.filter(index => pgLst(index) == ']')

  val closingBracketPositions = closingBracketIndexes.map(index => index-1)

  (openingBracketPositions.map(pos => pos-1 -> jumpRight(pg, pos, 0)) ++ closingBracketPositions.map(pos => pos+1 -> jumpLeft(pg, pos, 0))).toMap

}

def getIndexList(lst: List[Char], currentIndex: Int = 0): List[Int] = 
  lst match {
    case Nil => Nil
    case _ :: tl => currentIndex :: getIndexList(tl, currentIndex + 1)
}

def jumpRight(prog: String, pc: Int, level: Int) : Int = {
    if(pc >= prog.size){
        pc
    }
    else{
        prog(pc) match {
            case '[' => jumpRight(prog, pc+1, level+1)

            case ']' if level != 0 => jumpRight(prog, pc +1, level-1)

            case ']' if level == 0 => pc +1

            case _ => jumpRight(prog, pc + 1, level)
        }
    }
}

def jumpLeft(prog: String, pc: Int, level: Int) : Int = {
    if(pc < 0){
        pc
    }
    else{
        prog(pc) match {
            case ']' => jumpLeft(prog, pc-1, level+1)

            case '[' if level != 0 => jumpLeft(prog, pc-1, level-1)

            case '[' if level == 0 => pc +1

            case _ => jumpLeft(prog, pc - 1, level)
        }
    }
}

// testcase
//
// jtable("""+++++[->++++++++++<]>--<+++[->>++++++++++<<]>>++<<----------[+>.>.<+<]""")
// =>  Map(69 -> 61, 5 -> 20, 60 -> 70, 27 -> 44, 43 -> 28, 19 -> 6)


def compute2(pg: String, tb: Map[Int, Int], pc: Int, mp: Int, mem: Mem) : Mem = 
  (pc, mp, mem) match {
    case (pcVal, mpVal, memVal) if(pcVal >= pg.size || pcVal < 0) => memVal

    case (pcVal, mpVal, memVal) if(pcVal < pg.size || pcVal >= 0) => 
        pg(pcVal) match {
            case '>' => compute2(pg, tb, pcVal+1, mpVal+1, memVal)
            
            case '<' => compute2(pg, tb, pcVal+1, mpVal-1, memVal)

            case '+' => compute2(pg, tb, pcVal+1, mpVal, write(memVal, mpVal, sread(memVal, mpVal) + 1))

            case '-' => compute2(pg, tb, pcVal+1, mpVal, write(memVal, mpVal, sread(memVal, mpVal) - 1))

            case '.' => {
                print(sread(memVal, mpVal).toChar) 

                compute2(pg, tb, pcVal+1, mpVal, memVal)
            }

            case '[' => {
                sread(memVal, mpVal) match {
                    case 0 => compute2(pg, tb, tb.getOrElse(pcVal, -1), mpVal, memVal)

                    case _ => compute2(pg, tb, pcVal+1, mpVal, memVal)
                }
            }

            case ']' => {
                sread(memVal, mpVal) match {
                    case 0 => compute2(pg, tb, pcVal +1, mpVal, memVal)

                    case _ => compute2(pg, tb, tb.getOrElse(pcVal, -1), mpVal, memVal)
                }
            }
            case _ => compute2(pg, tb, pcVal+1, mpVal, memVal)
        }
    
}

def sread(mem: Mem, mp: Int) : Int = mem.getOrElse(mp, 0)

def write(mem: Mem, mp: Int, v: Int) : Mem = mem.updated(mp, v)

def run2(pg: String, m: Mem = Map()) = compute2(pg, jtable(pg), 0, 0, m)

// testcases
// time_needed(1, run2(load_bff("benchmark.bf")))
// time_needed(1, run2(load_bff("sierpinski.bf")))



// (7) 

def optimise(s: String) : String = s.replaceAll("""[^<>+\-.\[\]]""", "").replaceAll("""\[-\]""", "0")

def compute3(pg: String, tb: Map[Int, Int], pc: Int, mp: Int, mem: Mem) : Mem = 
  (pc, mp, mem) match {
  case (pcVal, mpVal, memVal) if(pcVal >= pg.size || pcVal < 0) => memVal

  case (pcVal, mpVal, memVal) if(pcVal < pg.size || pcVal >= 0) => 
      pg(pcVal) match {
          case '>' => compute3(pg, tb, pcVal+1, mpVal+1, memVal)
          
          case '<' => compute3(pg, tb, pcVal+1, mpVal-1, memVal)

          case '+' => compute3(pg, tb, pcVal+1, mpVal, write(memVal, mpVal, sread(memVal, mpVal) + 1))

          case '-' => compute3(pg, tb, pcVal+1, mpVal, write(memVal, mpVal, sread(memVal, mpVal) - 1))

          case '.' => {
              print(sread(memVal, mpVal).toChar) 

              compute3(pg, tb, pcVal+1, mpVal, memVal)
          }

          case '[' => {
              sread(memVal, mpVal) match {
                  case 0 => compute3(pg, tb, tb.getOrElse(pcVal, -1), mpVal, memVal)

                  case _ => compute3(pg, tb, pcVal+1, mpVal, memVal)
              }
          }

          case ']' => {
              sread(memVal, mpVal) match {
                  case 0 => compute3(pg, tb, pcVal +1, mpVal, memVal)

                  case _ => compute3(pg, tb, tb.getOrElse(pcVal, -1), mpVal, memVal)
              }
          }
          case '0' => compute3(pg, tb, pcVal + 1, mpVal, write(memVal, mpVal, 0))
            
          
          case _ => compute3(pg, tb, pcVal+1, mpVal, memVal)
      }
  }

def run3(pg: String, m: Mem = Map()) = compute3(optimise(pg), jtable(optimise(pg)), 0, 0, m)


// testcases
//
// optimise(load_bff("benchmark.bf"))          // should have inserted 0's
// optimise(load_bff("mandelbrot.bf")).length  // => 11205
// 
// time_needed(1, run3(load_bff("benchmark.bf")))



// (8)  
def combine(s: String) : String = ???

// testcase
// combine(load_bff("benchmark.bf"))

def compute4(pg: String, tb: Map[Int, Int], pc: Int, mp: Int, mem: Mem) : Mem = ???

// should call first optimise and then combine on the input string
//
def run4(pg: String, m: Mem = Map()) = ???


// testcases
// combine(optimise(load_bff("benchmark.bf"))) // => """>A+B[<A+M>A-A]<A[[....."""

// testcases (they should now run much faster)
// time_needed(1, run4(load_bff("benchmark.bf")))
// time_needed(1, run4(load_bff("sierpinski.bf"))) 
// time_needed(1, run4(load_bff("mandelbrot.bf")))


}
