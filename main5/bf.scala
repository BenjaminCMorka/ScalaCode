// Main Part 5 about an Interpreter for 
// the Brainf*** language
//==============================================


object M5a {

// representation of BF memory 

type Mem = Map[Int, Int]

import io.Source
import scala.util._


// (1)
def load_bff(name: String) : String = {
    Try (Source.fromFile(name, "ISO-8859-1").mkString).getOrElse("")
}

// (2) 

def sread(mem: Mem, mp: Int) : Int = mem.getOrElse(mp, 0)

def write(mem: Mem, mp: Int, v: Int) : Mem = mem.updated(mp, v)

// (3) 

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


// testcases
//jumpRight("""--[..+>--],>,++""", 3, 0)         // => 10
//jumpLeft("""--[..+>--],>,++""", 8, 0)          // => 3
//jumpRight("""--[..[+>]--],>,++""", 3, 0)       // => 12
//jumpRight("""--[..[[-]+>[.]]--],>,++""", 3, 0) // => 18
//jumpRight("""--[..[[-]+>[.]]--,>,++""", 3, 0)  // => 22 (outside)
//jumpLeft("""[******]***""", 7, 0)              // => -1 (outside)



// (4) 

def compute(prog: String, pc: Int, mp: Int, mem: Mem) : Mem = 
    (pc, mp, mem) match {
        case (pcVal, mpVal, memVal) if(pcVal >= prog.size || pcVal < 0) => memVal

        case (pcVal, mpVal, memVal) if(pcVal < prog.size && pcVal >= 0) => 
            prog(pcVal) match {
                case '>' => compute(prog, pcVal+1, mpVal+1, memVal)
                
                case '<' => compute(prog, pcVal+1, mpVal-1, memVal)

                case '+' => compute(prog, pcVal+1, mpVal, write(memVal, mpVal, sread(memVal, mpVal) + 1))

                case '-' => compute(prog, pcVal+1, mpVal, write(memVal, mpVal, sread(memVal, mpVal) - 1))

                case '.' => {
                    print(sread(memVal, mpVal).toChar) 

                    compute(prog, pcVal+1, mpVal, memVal)
                }

                case '[' => {
                    sread(memVal, mpVal) match {
                        case 0 => compute(prog, jumpRight(prog, pcVal+1, 0), mpVal, memVal)

                        case _ => compute(prog, pcVal+1, mpVal, memVal)
                    }
                }

                case ']' => {
                    sread(memVal, mpVal) match {
                        case 0 => compute(prog, pcVal +1, mpVal, memVal)

                        case _ =>compute(prog, jumpLeft(prog, pcVal-1, 0), mpVal, memVal)
                    }
                }
                case _ => compute(prog, pcVal+1, mpVal, memVal)
            }
        
    }

def run(prog: String, m: Mem = Map()) = compute(prog, 0, 0, m)

// (5)
def generate(msg: List[Char]) : String = msg.mkString.map(c => charToPlus(c) + ".[-]").mkString


def charToPlus(c: Char) : String = "+" * c.toInt




// some sample bf-programs collected from the Internet
//=====================================================


// some contrived (small) programs
//---------------------------------

// clears the 0-cell
//run("[-]", Map(0 -> 100))    // Map will be 0 -> 0

// moves content of the 0-cell to 1-cell
//run("[->+<]", Map(0 -> 10))  // Map will be 0 -> 0, 1 -> 10


// copies content of the 0-cell to 2-cell and 4-cell
//run("[>>+>>+<<<<-]", Map(0 -> 42))    // Map(0 -> 0, 2 -> 42, 4 -> 42)


// prints out numbers 0 to 9
//run("""+++++[->++++++++++<]>--<+++[->>++++++++++<<]>>++<<----------[+>.>.<+<]""")


// some more "useful" programs
//-----------------------------

// hello world program 1
//run("""++++++++[>++++[>++>+++>+++>+<<<<-]>+>+>->>+[<]<-]>>.>---.+++++++
//       ..+++.>>.<-.<.+++.------.--------.>>+.>++.""")

// hello world program 2
//run("""++++++++++[>+++++++>++++++++++>+++>+<<<<-]>++.>+.+++++++..+++.>+
//       +.<<+++++++++++++++.>.+++.------.--------.>+.>.""")

// hello world program 3
//run("""+++++++++[>++++++++>+++++++++++>+++++<<<-]>.>++.+++++++..
//       +++.>-.------------.<++++++++.--------.+++.------.--------.>+.""")

 
// draws the Sierpinski triangle
//run(load_bff("sierpinski.bf"))


//fibonacci numbers below 100
//run("""+++++++++++
//      >+>>>>++++++++++++++++++++++++++++++++++++++++++++
//      >++++++++++++++++++++++++++++++++<<<<<<[>[>>>>>>+>
//      +<<<<<<<-]>>>>>>>[<<<<<<<+>>>>>>>-]<[>++++++++++[-
//      <-[>>+>+<<<-]>>>[<<<+>>>-]+<[>[-]<[-]]>[<<[>>>+<<<
//      -]>>[-]]<<]>>>[>>+>+<<<-]>>>[<<<+>>>-]+<[>[-]<[-]]
//      >[<<+>>[-]]<<<<<<<]>>>>>[+++++++++++++++++++++++++
//      +++++++++++++++++++++++.[-]]++++++++++<[->-<]>++++
//      ++++++++++++++++++++++++++++++++++++++++++++.[-]<<
//      <<<<<<<<<<[>>>+>+<<<<-]>>>>[<<<<+>>>>-]<-[>>.>.<<<
//      [-]]<<[>>+>+<<<-]>>>[<<<+>>>-]<<[<+>-]>[<+>-]<<<-]""")

//outputs the square numbers up to 10000
//run("""++++[>+++++<-]>[<+++++>-]+<+[>[>+>+<<-]++>>[<<+>>-]>>>[-]++>[-]+
//       >>>+[[-]++++++>>>]<<<[[<++++++++<++>>-]+<.<[>----<-]<]
//       <<[>>>>>[>>>[-]+++++++++<[>-<-]+++++++++>[-[<->-]+[<<<]]<[>+<-]>]<<-]<<-]""")


// calculates 2 to the power of 6 
//(example from a C-to-BF compiler at https://github.com/elikaski/BF-it)
//run(""">>[-]>[-]++>[-]++++++><<<>>>>[-]+><>[-]<<[-]>[>+<<+>-]>[<+>-]
//       <><[-]>[-]<<<[>>+>+<<<-]>>>[<<<+>>>-][-]><<>>[-]>[-]<<<[>>[-]
//       <[>+>+<<-]>[<+>-]+>[[-]<-<->>]<<<-]>>[<<+>>-]<<[[-]>[-]<<[>+>
//       +<<-]>>[<<+>>-][-]>[-]<<<<<[>>>>+>+<<<<<-]>>>>>[<<<<<+>>>>>-]
//       <<>>[-]>[-]<<<[>>>+<<<-]>>>[<<[<+>>+<-]>[<+>-]>-]<<<>[-]<<[-]
//       >[>+<<+>-]>[<+>-]<><[-]>[-]<<<[>>+>+<<<-]>>>-[<<<+>>>-]<[-]>[-]
//       <<<[>>+>+<<<-]>>>[<<<+>>>-][-]><<>>[-]>[-]<<<[>>[-]<[>+>+<<-]>
//       [<+>-]+>[[-]<-<->>]<<<-]>>[<<+>>-]<<][-]>[-]<<[>+>+<<-]>>[<<+>
//       >-]<<<<<[-]>>>>[<<<<+>>>>-]<<<<><>[-]<<[-]>[>+<<+>-]>[<+>-]<>
//       <[-]>[-]>[-]<<<[>>+>+<<<-]>>>[<<<+>>>-]<<>>[-]>[-]>[-]>[-]>[-]>
//       [-]>[-]>[-]>[-]>[-]<<<<<<<<<<>>++++++++++<<[->+>-[>+>>]>[+[-<+
//       >]>+>>]<<<<<<]>>[-]>>>++++++++++<[->-[>+>>]>[+[-<+>]>+>>]<<<<<
//       ]>[-]>>[>++++++[-<++++++++>]<.<<+>+>[-]]<[<[->-<]++++++[->++++
//       ++++<]>.[-]]<<++++++[-<++++++++>]<.[-]<<[-<+>]<<><<<""")



// a Mandelbrot set generator in brainf*** written by Erik Bosman
// (http://esoteric.sange.fi/brainfuck/utils/mandelbrot/)
//
//run(load_bff("mandelbrot.bf"))


// a benchmark program (counts down from 'Z' to 'A')
//
//run(load_bff("benchmark.bf"))

// calculates the Collatz series for numbers from 1 to 30
//
//run(load_bff("collatz.bf"))

}
