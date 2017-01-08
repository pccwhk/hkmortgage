package org.ccw

import org.apache.poi.ss.formula.functions.FinanceLib

object Mortgage {
  def pmt(rateInBps: Int, year: Double, p: Double, isEndOfperiod: Boolean): Double = {
    val r = rateInBps / 120000.0
    val n = year * 12
    FinanceLib.pmt(r, n, -p, 0, isEndOfperiod)
  }
 
  def pmt(rateInBps: Int, year: Int, p: Double, isEndOfperiod: Boolean): Double = {
    pmt(rateInBps, year.toDouble, p, isEndOfperiod)
  }

  def getInsurance(loanAmt: Double, years: Int, flatValue: Double): Double = {
    val debtRatio = loanAmt / flatValue
    getInsurance(debtRatio, years)
  }

  def geTotalLoanAmt(flatPrice: Double, loanRatio: Int, years: Int): Double = {
    val loanAmt = (flatPrice * loanRatio) / 100.0
    val total = loanAmt + (getInsurance(loanRatio, years) * loanAmt * 0.8) / 100.0
    //println(s"$total, $loanAmt")
    total
  }

  def getInsurance(debtRatio: Double, years: Int): Double = {
    debtRatio match {
      case x if (x <= 80 && x >= 60) => {
        years match {
          case 10                     => 1.15
          case x if x <= 15 && x > 10 => 1.5
          case x if x <= 20 && x > 15 => 1.85
          case x if x <= 25 && x > 20 => 2.00
          case x if x <= 30 && x > 25 => 2.15
        }
      }
      case x if (x <= 85 && x > 80) => {
        years match {
          case 10                     => 1.80
          case x if x <= 15 && x > 10 => 2.28
          case x if x <= 20 && x > 15 => 2.68
          case x if x <= 25 && x > 20 => 2.95
          case x if x <= 30 && x > 25 => 3.05
        }
      }
      case x if (x <= 90 && x > 85) => {
        years match {
          case 10                     => 2.6
          case x if x <= 15 && x > 10 => 3.18
          case x if x <= 20 && x > 15 => 3.75
          case x if x <= 25 && x > 20 => 4.1
          case x if x <= 30 && x > 25 => 4.35
        }
      }
      case x if (x <= 60) => 0
      case _ => {
        throw new Exception()
        //println("I am being called!!") 
        0
      }
    }
  }
}
 
object StressTest {
  val STRESS_TEST_RATE_BPS = 300
  val STRESS_TEST_LOAN_RATIO = 80

}
 
case class MortgagePayment(years: Int, rateInBps: Int, flatPrice: Double,
                           loanAmt: Double, totalLoanAmt: Double, monthlyPayment: Double, loanRatio :Int) {

  //val loanRatio = loanAmt / flatPrice
  val stressRate = rateInBps + StressTest.STRESS_TEST_RATE_BPS

  val stressMonthly = Mortgage.pmt(stressRate, years, totalLoanAmt, false)

  val minSalary: Double = {
    if (loanRatio <= StressTest.STRESS_TEST_LOAN_RATIO) {
      Math.max(monthlyPayment / 0.5, stressMonthly / 0.6)
    } else {
      Math.max(monthlyPayment / 0.45, stressMonthly / 0.55)
    }
  }

  override def toString(): String = {
    //s"$years, $rate, $flatPrice, $loanAmt, $monthlyPayment, min Salary = $minSalary for stress Rate = $stressRate"
    s"$years, ${rateInBps/10000.0}, $flatPrice, $loanAmt, $monthlyPayment, $minSalary, $stressRate, $totalLoanAmt"
  }
}