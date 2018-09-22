package frdomain.ch5
package free

import scalaz.Free
import scalaz.concurrent.Task

object Main {

  import common._

  object app1 {

    import AccountService._

    val composite: Free[AccountRepoF, Account] =
      for {
        x <- open("a-123", "debasish ghosh", Some(today))
        _ <- credit(x.no, 10000)
        _ <- credit(x.no, 30000)
        _ <- debit(x.no, 23000)
        a <- query(x.no)
      } yield a

    val t = AccountRepoMutableInterpreter().apply(composite)
  }

  object app2 {

    import AccountRepository._

    val account = Account("a-123", "John K")
    val composite: Free[AccountRepoF, Account] = for {
      _ <- store(account.copy(balance = Balance(1000)))
      _ <- store(account.copy(no = "b-123", balance = Balance(1234)))
      _ <- delete(account.no)
      c <- query("b-123")
    } yield c

    val u: Task[Account] = AccountRepoMutableInterpreter().apply(composite)

    val v: List[String] = AccountRepoShowInterpreter().interpret(composite, List.empty)
  }

  def main(args: Array[String]): Unit = {
    println(app1.t.unsafePerformSync)
    println(app2.u.unsafePerformSync)
    println(app2.v)
  }
}
