package recfun
import common._

object Main {
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }
    print(countChange(5,List(1,2,3,4,5), Nil))
  }

  /**
   * Exercise 1
   */
  def pascal(c: Int, r: Int): Int = {
    if (c == 0 || c == r) 1
    else pascal(c-1, r-1) + pascal(c, r-1)
  }

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    def balanceStr(chars: List[Char], state: Int ): Boolean = {
      if (chars.isEmpty)
        state == 0
      else {
        chars.head match {
          case '(' => balanceStr(chars.tail, state + 1)
          case ')' => state match {
            case 0 => false
            case _ => balanceStr(chars.tail, state - 1)
          }
          case _ => balanceStr(chars.tail, state)
        }
      }
    }
    balanceStr(chars, 0)
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int], change: List[Int]): Int = {
    if (coins.isEmpty) {
      money match {
        case 0 => {
          println(change)
          1
        }
        case _ => 0
      }
    }
    else {
      money match {
        case 0 => {
          println(change)
          1
        }
        case x if x < 0 => 0
        case x if x > 0 => countChange(money, coins.tail, change) + countChange(money - coins.head, coins, coins.head :: change)
      }
    }
  }
}
