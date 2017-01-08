

package org.ccw

object Main {

  def printall(l: Seq[Any]) = l.foreach(println)
  def printall(l: (Any, Any)) = println(l)

  def main(args: Array[String]): Unit = {

    val oustandingLoan = 2935635.0
    val month = 11418
    val MAX_SALARY = 34000.0

    val allData: Seq[MortgagePayment] = for (
      years <- 20 to 30 by 1;
      rateInBps <- 215 to 500 by 25;
      flatPrice <- 3700000 to 4500000 by 10000;
      loanRatio <- 60 to 90 by 1;
      loanAmt = (flatPrice * loanRatio) / 100.0;
      totalLoan = Mortgage.geTotalLoanAmt(flatPrice, loanRatio, years)
    ) yield MortgagePayment(years, rateInBps, flatPrice, loanAmt, totalLoan, Mortgage.pmt(rateInBps, years,
      totalLoan, false), loanRatio)

    //allData.filter(x =>  x.years == 30 && x.loanRatio == 79 && x.rateInBps == 215).foreach(println)
    println("************")
    
    val good: Seq[(Double, Double, Double, MortgagePayment)] = for (
      m <- allData;
      investReturn <- 40 to 50 by 1;
      //if m.minSalary <= MAX_SALARY;
      extraCash = m.loanAmt - oustandingLoan;
      if extraCash >= 0;
      annualExtraReturn = (extraCash * investReturn) / 1000.0; // divide by 12 months and divide rate by 1000
      orgPay = Mortgage.pmt(m.rateInBps, m.years , oustandingLoan, false);
      investReturnRate = (investReturn / 1000.0);
      rebate = m.loanAmt * 0.6 * 0.012;  // 60% of loan amt can get rebate of 1.2%
      gain = annualExtraReturn * m.years
          + rebate  
//          - (m.monthlyPayment - orgPay) * 24   // extra monthly payment diff x 12 x 2 years (short term)
        - ((m.monthlyPayment - orgPay) *12) * (m.years)  // extra monthly payment diff x 12 x (n-2) years
        //- m.monthlyPayment * 24;                          // extra monthly payment x 12 x 2 years
      if gain > 0
    ) yield (gain, investReturnRate, extraCash, m)

    //printall(good)
    println("************")
    printall(good.groupBy(_._1)
      .reduceLeft((x, y) => if (x._1 > y._1) x else y)._2)
    /*
    val r = list.filter(x => true).
    filter(x => x.loanRatio <= 0.8 && x.loanRatio > 0.7).
    filter(_.years == 30).
    filter(_.flatPrice == 3800000).
    filter(_.minSalary <= 35000).
    groupBy(_.minSalary).reduceLeft( (x, y) => if (x._1 > y._1) x else y) // highest MinSalary
    //groupBy(_.minSalary).reduceLeft( (x, y) => if (x._1 > y._1) y else x) // lowest MinSalary
    printall(r)*/

  }

}


