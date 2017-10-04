package recfun

object Main {
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }
  }

  /**
   * Exercise 1
   */
    def pascal(c: Int, r: Int): Int = {
      if(c==0 || c == r) 1
      else if (c > r) 0
      else pascal(c, r-1) + pascal(c-1, r-1)
    }
  
  /**
   * Exercise 2
   */
    def balance(chars: List[Char]): Boolean = {
      def f(L:List[Char], stack:List[Char]) :Boolean = {
        if (L.isEmpty && stack.isEmpty) true
        else if (L.isEmpty && !stack.isEmpty) false
        else if (L.head == '(') f(L.tail, '(' :: stack)
        else if (L.head == ')' && (stack.isEmpty || stack.head != '(')) false
        else if (L.head == ')' && (!stack.isEmpty && stack.head == '(')) f(L.tail, stack.tail)
        else f(L.tail, stack)
      }
      f(chars, List[Char]())
    }
  
  /**
   * Exercise 3
   */
    def countChange(money: Int, coins: List[Int]): Int = {
        if(money < 0) 0
        else if(coins.isEmpty)
          if(money == 0) 1 else 0
        else
          countChange(money, coins.tail) + countChange(money - coins.head, coins)
      }

  }
