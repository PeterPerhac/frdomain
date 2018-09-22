package frdomain.ch5
package free

import java.util.Date

import frdomain.ch5.free.common._
import scalaz.Scalaz._
import scalaz._

trait AccountService[Account, Amount, Balance] {
  def open(no: String, name: String, openingDate: Option[Date]): AccountRepo[Account]

  def close(no: String, closeDate: Option[Date]): AccountRepo[Account]

  def debit(no: String, amount: Amount): AccountRepo[Account]

  def credit(no: String, amount: Amount): AccountRepo[Account]

  def balance(no: String): AccountRepo[Balance]
}

object AccountService extends AccountService[Account, Amount, Balance] with AccountRepository {

  def open(no: String, name: String, openingDate: Option[Date]): Free[AccountRepoF, Account] =
    store(Account(no, name, openingDate.get)) >> query(no)

  private val close: Account => Account =
    _.copy(dateOfClosing = Some(today))

  def close(no: String, closeDate: Option[Date]): AccountRepo[Account] =
    update(no, close) >> query(no)

  private def debitImpl(amount: Amount)(a: Account): Account = {
    if (a.balance.amount < amount) throw new RuntimeException("insufficient funds")
    a.copy(balance = Balance(a.balance.amount - amount))
  }

  def debit(no: String, amount: Amount): AccountRepo[Account] =
    updateBalance(no, amount, debitImpl) >>= (_ => query(no))

  private def creditImpl(amount: Amount)(a: Account): Account =
    a.copy(balance = Balance(a.balance.amount + amount))

  def credit(no: String, amount: Amount): AccountRepo[Account] =
    updateBalance(no, amount, creditImpl) >>= (_ => query(no))

  def balance(no: String): AccountRepo[Balance] =
    query(no).map(_.balance)

}
