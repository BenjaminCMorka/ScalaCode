// Shunting Yard Algorithm
// by Edsger Dijkstra
// ========================

object C3a {

// type of tokens
type Toks = List[String]

// the operations in the basic version of the algorithm
val ops = List("+", "-", "*", "/")

// the precedences of the operators
val precs = Map("+" -> 1,
		"-" -> 1,
		"*" -> 2,
		"/" -> 2)

// helper function for splitting strings into tokens
def split(s: String) : Toks = s.split(" ").toList

// ADD YOUR CODE BELOW
//======================


// (1) 
def is_op(op: String) : Boolean = ops.contains(op)

def prec(op1: String, op2: String) : Boolean = precs.getOrElse(op1, 0) >= precs.getOrElse(op2, 0)

def syard(toks: Toks, st: Toks = Nil, out: Toks = Nil) : Toks = 
	toks match {
		case Nil => out ++ st.filter(is_op)

		case x::xs if x.forall(_.isDigit) => syard(xs, st, out:+ x)

		case x::xs if is_op(x) && st.nonEmpty && prec(st.head, x) => syard(x::xs, st.tail, out:+ st.head)

		case x::xs if is_op(x) && (st.isEmpty || !prec(st.head, x)) => syard(xs, x::st, out)

		case x::xs if x == "(" => syard(xs, x::st, out)

		case x::xs if x == ")" => {
			st match {
				
				case hd::tl if hd == "(" => syard(xs, tl, out) 

				case hd::tl if is_op(hd) => syard(x::xs, tl, out:+ hd) 

				case _  => out
			}
			
		}
		case _ => out
	}


// test cases
//syard(split("3 + 4 * ( 2 - 1 )"))  // 3 4 2 1 - * +
//syard(split("10 + 12 * 33"))       // 10 12 33 * +
//syard(split("( 5 + 7 ) * 2"))      // 5 7 + 2 *
//syard(split("5 + 7 / 2"))          // 5 7 2 / +
//syard(split("5 * 7 / 2"))          // 5 7 * 2 /
//syard(split("9 + 24 / ( 7 - 3 )")) // 9 24 7 3 - / +

//syard(split("3 + 4 + 5"))           // 3 4 + 5 +
//syard(split("( ( 3 + 4 ) + 5 )"))    // 3 4 + 5 +
//syard(split("( 3 + ( 4 + 5 ) )"))    // 3 4 5 + +
//syard(split("( ( ( 3 ) ) + ( ( 4 + ( 5 ) ) ) )")) // 3 4 5 + +



// (2) 
def compute(toks: Toks, st: List[Int] = Nil) : Int =
	toks match {
		case Nil => if (st.length == 1) st.head else -1

		case x::xs if x.forall(_.isDigit) => compute(xs, x.toInt :: st)

		case x::xs if x == "+" => {
			val num1 = st.head
			val num2 = st.tail.head

			val restOfList = st.tail.tail
			compute(xs, (num2 + num1) :: restOfList)
		}

		case x::xs if x == "-" => {
			val num1 = st.head
			val num2 = st.tail.head

			val restOfList = st.tail.tail
			compute(xs, (num2 - num1)::restOfList)
		}

		case x::xs if x == "*" => {
			val num1 = st.head
			val num2 = st.tail.head

			val restOfList = st.tail.tail
			compute(xs, (num2 * num1)::restOfList)
		}

		case x::xs if x == "/" => {
			val num1 = st.head
			val num2 = st.tail.head

			val restOfList = st.tail.tail
			compute(xs, (num2 / num1)::restOfList)
		}

		case _ => -1
	}
	


// test cases
// compute(syard(split("3 + 4 * ( 2 - 1 )")))  // 7
// compute(syard(split("10 + 12 * 33")))       // 406
// compute(syard(split("( 5 + 7 ) * 2")))      // 24
// compute(syard(split("5 + 7 / 2")))          // 8
// compute(syard(split("5 * 7 / 2")))          // 17
// compute(syard(split("9 + 24 / ( 7 - 3 )"))) // 15

}


