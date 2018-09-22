package frdomain.ch5
package free

import scalaz.Free
import common._
import scalaz.Free.liftF

sealed trait AccountRepoF[A]

case class Query(no: String) extends AccountRepoF[Account]

case class Store(account: Account) extends AccountRepoF[Unit]

case class Delete(no: String) extends AccountRepoF[Unit]

trait AccountRepository {
  def store(account: Account): AccountRepo[Unit] =
    liftF(Store(account))

  def query(no: String): AccountRepo[Account] =
    liftF(Query(no))

  def delete(no: String): AccountRepo[Unit] =
    liftF(Delete(no))

  def update(no: String, f: Account => Account): AccountRepo[Unit] =
    query(no) >>= (f andThen store)

  def updateBalance(no: String, amount: Amount, f: Amount => Account => Account): Free[AccountRepoF, Unit] =
    query(no) >>= (f(amount) andThen store)
}

object AccountRepository extends AccountRepository

