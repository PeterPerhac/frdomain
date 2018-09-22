package frdomain.ch5
package free

import scalaz.Scalaz._
import scalaz._
import scalaz.concurrent.Task
import scalaz.concurrent.Task.{fail, now}

import scala.collection.mutable.{Map => MMap}

trait AccountRepoInterpreter {
  def apply[A](action: AccountRepo[A]): Task[A]
}

/**
  * Basic interpreter that uses a global mutable Map to store the state
  * of computation
  */
case class AccountRepoMutableInterpreter() extends AccountRepoInterpreter {
  val table: MMap[String, Account] = MMap.empty[String, Account]

  val interpreter: AccountRepoF ~> Task = new (AccountRepoF ~> Task) {
    override def apply[A](fa: AccountRepoF[A]): Task[A] = fa match {
      case Query(no) => table.get(no).fold[Task[Account]](fail(new RuntimeException(s"Account no $no not found")))(now)
      case Store(account) => now(table += ((account.no, account))).void
      case Delete(no) => now(table -= no).void
    }
  }

  /**
    * Turns the AccountRepo script into a `Task` that executes it in a mutable setting
    */
  def apply[A](action: AccountRepo[A]): Task[A] = action.foldMap(interpreter)
}

case class AccountRepoShowInterpreter() {

  type ListState[A] = State[List[String], A]
  val interpreter: AccountRepoF ~> ListState = new (AccountRepoF ~> ListState) {
    private def show(s: String): ListState[Unit] = State(l => (l ++ List(s), ()))

    override def apply[A](fa: AccountRepoF[A]): ListState[A] = fa match {
      case Query(no) => show(s"Query for $no").map(_ => Account(no, ""))
      case Store(account) => show(s"Storing $account")
      case Delete(no) => show(s"Deleting $no")
    }
  }

  def interpret[A](script: AccountRepo[A], ls: List[String]): List[String] =
    script.foldMap(interpreter).exec(ls)

}
