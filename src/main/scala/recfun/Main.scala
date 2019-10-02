package recfun

object Main {
  def main(args: Array[String]) {
    //    println("Pascal's Triangle")
    //    for (row <- 0 to 10) {
    //      for (col <- 0 to row)
    //        print(pascal(col, row) + " ")
    //      println()
    //    }
    //    println(balance("(a)(()))".toCharArray.toList))
    println(countChange(4, List(1, 2)))
  }

  /**
    * Exercise 1
    */
  def pascal(c: Int, r: Int): Int = {
    if (c == 0 && r == 0) 1
    else if (r < 0 || c < 0) 0
    else pascal(c - 1, r - 1) + pascal(c, r - 1)
  }

  /**
    * Exercise 2
    */
  def balance(chars: List[Char]): Boolean = balanceReq(0, chars)


  def balanceReq(count: Int, chars: List[Char]): Boolean = {
    if (chars.isEmpty) {
      if (count == 0) true
      else false
    } else {
      if (chars.head == '(') {
        balanceReq(count + 1, chars.tail)
      } else if (chars.head == ')') {
        if (count > 0) {
          balanceReq(count - 1, chars.tail)
        } else {
          false
        }
      } else {
        balanceReq(count, chars.tail)
      }
    }
  }

  /**
    * Exercise 3
    */
  def countChange(money: Int, coins: List[Int]): Int = {

    def recSum(coinsR: List[Int], curSum: Int = 0): Int = {
      val c = coinsR.head + curSum
      if (c == money) {
        1 + recSum(coinsR.tail, curSum)
      } else if (c > money) {
        0 + recSum(coinsR.tail, curSum)
      } else {
        recSum(coinsR, c)
      }
    }

    recSum(coins)
  }

}
