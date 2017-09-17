package org.coursera.epl.functional.design.week3.stateful

object stateful {
  var count = 111
  //can be rewritten
  count = 10


  val account = new BankAccount
  account.deposit(100)
  account.withdraw(300)
  
  class BankAccountProxy(ba: BankAccount) {
    def deposit(amount: Int): Unit = ba.deposit(amount)

    def withdraw(amount: Int): Int = ba.withdraw(amount)
  }


}