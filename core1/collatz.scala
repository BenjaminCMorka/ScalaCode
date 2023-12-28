// Core Part 1 about the 3n+1 conjecture
//============================================

object C1 {

// ADD YOUR CODE BELOW
//======================


//(1) 
def collatz(n: Long) : Long = {
    if (n == 1) {
        0
    } 
    else if (is_even(n)) { 
        collatz(n / 2) + 1
    } 
    else { 
      collatz( (n * 3) + 1) + 1
    }
}


//(2) 
def collatz_max(bnd: Long) : (Long, Long) = {
    val resultsList = (1L to bnd).map(collatz).toList

    val maxSteps = resultsList.max

    val correspondingNum = resultsList.indexOf(maxSteps).toLong + 1

    (maxSteps, correspondingNum)
}

//(3)
def is_pow_of_two(n: Long) : Boolean = (n & (n - 1)) == 0

def is_hard(n: Long) : Boolean = is_pow_of_two((n *3) + 1)

def last_odd(n: Long) : Long = {
    if (is_hard(n)){
        n
    }
    else{
        if( is_even(n)){
            last_odd(n/2)
        }
        
        else{
            last_odd((n * 3) + 1)
        }
    }
}

def is_even(n: Long) : Boolean = n % 2 == 0

}

