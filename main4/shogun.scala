// Main Part 4 about the Shogun Board Game
//=========================================

// Task 1 - 5 see below

object M4 {   

type Pos = (Int, Int)    // a position on a chessboard 

// Colours: Red or White
abstract class Colour
case object Red extends Colour
case object Wht extends Colour

// Pieces: Either Pawns or Kings
//===============================
abstract class Piece {
  def pos : Pos       
  def col : Colour    
  def en : Int      // energy for Pawns 1 - 4, for Kings 1 - 2
}
case class Pawn(en: Int, col: Colour, pos: Pos) extends Piece
case class King(en: Int, col: Colour, pos: Pos) extends Piece

// how to extract components from pieces
//val p = Pawn(4, Wht, (3,2))
//assert(p.pos == (3,2))
//assert(p.col == Wht)
//assert(p.en == 4)  

// checks if a piece is a king
def is_king(pc: Piece) : Boolean = pc match {
  case King(_, _, _) => true
  case _ => false
}

// incrementing and decrementing the position of a piece
def incx(pc: Piece) : Piece = pc match {
  case Pawn(en, c, (x,y)) => Pawn(en, c, (x+1,y))
  case King(en, c, (x,y)) => King(en, c, (x+1,y))
}

def incy(pc: Piece) : Piece = pc match {
  case Pawn(en, c, (x,y)) => Pawn(en, c, (x,y+1))
  case King(en, c, (x,y)) => King(en, c, (x,y+1))
}

def decx(pc: Piece) : Piece = pc match {
  case Pawn(en, c, (x,y)) => Pawn(en, c, (x-1,y))
  case King(en, c, (x,y)) => King(en, c, (x-1,y))
}

def decy(pc: Piece) : Piece = pc match {
  case Pawn(en, c, (x,y)) => Pawn(en, c, (x,y-1))
  case King(en, c, (x,y)) => King(en, c, (x,y-1))
}

//pretty printing colours and pieces
def pp_color(c: Colour) : String = c match {
  case Red => "R"
  case Wht => "W"
}

def pp(pc: Piece) : String = pc match {
  case Pawn(n, c, _) => s"P${pp_color(c)}$n"
  case King(n, c, _) => s"K${pp_color(c)}$n"
}

// Boards are sets of pieces
//===========================
case class Board(pces: Set[Piece]) {
  def +(pc: Piece) : Board = Board(pces + pc)
  def -(pc: Piece) : Board = Board(pces - pc)
}

// checking whether a position is occupied in a board
def occupied(p: Pos, b: Board) : Option[Piece] =  
  b.pces.find(p == _.pos)
  
def occupied_by(p: Pos, b: Board) : Option[Colour] =
  occupied(p, b).map(_.col)

def is_occupied(p: Pos, b: Board) : Boolean =
  occupied(p, b).isDefined

// is a position inside a board
def inside(p: Pos, b: Board): Boolean = 
  1 <= p._1 && 1 <= p._2 && p._1 <= 8 && p._2 <= 8 

// pretty printing a board
def print_board(b: Board): Unit = {
  println()
  for (i <- 8 to 1 by -1) {
    println("----" * 8)
    for (j <- 1 to 8) {
      val opc = occupied((j,i), b)
      if (opc.isDefined) print(s"|${pp(opc.get)}") 
      else print("|   ")
    }
    println("|")
  } 
  println("----" * 8)
}

// example board: initial board
val b_init = Board(Set(King(2,Wht,(4,1)), King(1,Red,(5,8)),
                  		 Pawn(4,Wht,(1,1)), Pawn(4,Red,(1,8)),
                  		 Pawn(3,Wht,(2,1)), Pawn(2,Red,(2,8)),
                  		 Pawn(2,Wht,(3,1)), Pawn(3,Red,(3,8)),
                  		 Pawn(1,Wht,(5,1)), Pawn(1,Red,(4,8)),
                  		 Pawn(4,Wht,(6,1)), Pawn(3,Red,(6,8)),
                  		 Pawn(3,Wht,(7,1)), Pawn(1,Red,(7,8)),
                  		 Pawn(2,Wht,(8,1)), Pawn(3,Red,(8,8))))

//print_board(b_init)
// --------------------------------
// |PR4|PR2|PR3|PR1|KR1|PR3|PR1|PR3|
// --------------------------------
// |   |   |   |   |   |   |   |   |
// --------------------------------
// |   |   |   |   |   |   |   |   |
// --------------------------------
// |   |   |   |   |   |   |   |   |
// --------------------------------
// |   |   |   |   |   |   |   |   |
// --------------------------------
// |   |   |   |   |   |   |   |   |
// --------------------------------
// |   |   |   |   |   |   |   |   |
// --------------------------------
// |PW4|PW3|PW2|KW2|PW1|PW4|PW3|PW2|
// --------------------------------




// Moves
//=======
abstract class Move
case object U extends Move    // up
case object D extends Move    // down
case object R extends Move    // right
case object L extends Move    // left
case object RU extends Move   // first right, then possibly up
case object LU extends Move   // first left, then possibly up
case object RD extends Move   // ...
case object LD extends Move
case object UR extends Move
case object UL extends Move
case object DR extends Move
case object DL extends Move

//======================
// ADD YOUR CODE BELOW
//======================


// Task 1: 
def eval(pc: Piece, m: Move, en: Int, b: Board) : Set[Piece] = 
  (pc.pos, en) match {
    case (p, _) if !inside(p, b) => Set()

    case (p, 0) if !is_occupied(p, b) => Set(pc)

    case (p, 0) if is_occupied(p, b) => if((occupied_by(p, b) != Some(pc.col))) Set(pc) else Set()

    case (p, n) if n > 0 && is_occupied(p, b) => Set()

    case (_, _) => 
      m match {
        case U => eval(incy(pc), U, en-1, b)

        case D => eval(decy(pc), D, en -1, b)

        case R => eval(incx(pc), R, en-1, b)

        case L => eval(decx(pc), L, en-1, b)

        case RU => eval(incx(pc), RU, en-1, b) ++ eval(pc, U, en, b)

        case LU => eval(decx(pc), LU, en-1, b) ++ eval(pc, U, en, b)

        case RD => eval(incx(pc), RD, en-1, b) ++ eval(pc, D, en, b)

        case LD => eval(decx(pc), LD, en-1, b) ++ eval(pc, D, en, b)

        case UR => eval(incy(pc), UR, en-1, b) ++ eval(pc, R, en, b)

        case UL => eval(incy(pc), UL, en-1, b) ++ eval(pc, L, en, b)

        case DR => eval(decy(pc), DR, en-1, b) ++ eval(pc, R, en, b)

        case DL => eval(decy(pc), DL, en-1, b) ++ eval(pc, L, en, b)
      }
  }







// Task 2: 
def all_moves(pc: Piece, b: Board) : Set[Piece] = {
  Set(U, D, R, L, RU, LU, RD, LD, UR, UL, DR, DL).flatMap(m => eval(pc, m, pc.en, b-pc))
}




// Task 3: 
def attacked(c: Colour, b: Board) : Set[Piece] = {
  val oppositePieces = b.pces.partition(_.col != c)._1

  oppositePieces.partition(isAttacked(_, b, c))._1
  
}

def isAttacked(pc: Piece, b: Board, c: Colour) : Boolean = {
  b.pces.count(attacker => all_moves(attacker, b).count( attacked => attacker.col == c && attacked.pos == pc.pos ) > 0) > 0
}

// Task 4: 
def attackedN(pc: Piece, b: Board) : Int = {
  val oppositePieces = b.pces.partition(_.col != pc.col)._1

  oppositePieces.count(p => all_moves(p, b)
    .count(tempPc => tempPc.pos == pc.pos) > 0
  )
}


// Task 5: 
def protectedN(pc: Piece, b: Board) : Int = {

  val samePieces = b.pces.partition(_.col == pc.col)._1

  samePieces.count(protectPiece => all_moves(protectPiece, b - pc)
    .count(tempPc => tempPc.pos == pc.pos) > 0
  )
}


// Task 6: 
def legal_moves(pc: Piece, b: Board) : Set[Piece] = {
  if (!is_king(pc)) {
    all_moves(pc, b)
  } 
  else {
    val notAttacked = all_moves(pc, b).partition(attackedN(_, b) == 0)._1
    notAttacked.partition(protectBlocks(_, b) < 1)._1
  }
}

def protectBlocks(pc: Piece, b: Board) : Int = {
  val oppositePieces = b.pces.partition(_.col != pc.col)._1
  oppositePieces.count (p => ((p.pos == pc.pos) && protectedN(p,b) > 0))
}



/*
// more test cases
//=================
val pw1 = Pawn(4, Wht, (4,6))
val pw2 = Pawn(4, Wht, (2,4))
val pw3 = Pawn(3, Red, (6,8))
val pw4 = Pawn(2, Red, (2,8))
val bt = b_init + pw1 + pw2

print_board(bt)
println(s"Capture Red: ${attacked(Wht, bt)}")
  // => Set(Pawn(2,Red,(2,8)), Pawn(3,Red,(6,8)))

println(s"Capture Wht: ${attacked(Red, bt)}")
  // => Set(Pawn(4,Wht,(4,6)))

println(s"ProtectedN:  ${protectedN(pw3, bt)}")
  // => 2

println(s"AttackedN:   ${attackedN(pw4, bt)}")
  // => 2

println(s"all moves:   ${all_moves(pw2, bt)}")
  // => Set(Pawn(4,Wht,(4,2)), Pawn(4,Wht,(1,7)), Pawn(4,Wht,(5,3)), Pawn(4,Wht,(5,5)), 
  //        Pawn(4,Wht,(2,8)), Pawn(4,Wht,(3,7)), Pawn(4,Wht,(6,4)))
*/

}