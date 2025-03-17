using Microsoft.VisualStudio.TestTools.UnitTesting;
using SampleUnitTests.Exception;
using SampleUnitTests.Model;

namespace SampleUnitTests;

[TestClass]
public class SavingsAccountTests {

    [TestMethod]
    public void SavingsAccount_Created_With_All_Initial_Values_Provided()  {

        // Arrange
        SavingsAccount testSavingsAcct;
        string testOwner              = "Tony Stark";
        double testStartingBalance    = 100.00;
        double testAnnualInterestRate = .03;

        // Act
        testSavingsAcct = new SavingsAccount(testOwner, testStartingBalance, testAnnualInterestRate);

        // Assert
        Assert.AreEqual(testOwner, testSavingsAcct.AcctOwner,"Account Owner not stored correctly");
        Assert.AreEqual(testStartingBalance, testSavingsAcct.GetBalance(),"Start balance not stored correctly");
        Assert.AreEqual(testAnnualInterestRate, testSavingsAcct.GetAnnualInterestRate(),"Annual interest rate not stored correctly");
        }

[TestMethod]
    public void SavingsAccount_Created_With_Only_Owner_And_Balance_Provided()  {

        // Arrange
        SavingsAccount testSavingsAcct;
        string testOwner              = "Tony Stark";
        double testStartingBalance    = 100.00;
        double testAnnualInterestRate = SavingsAccount.SavingsDefaultApr;

        // Act
        testSavingsAcct = new SavingsAccount(testOwner, testStartingBalance);

        // Assert
        Assert.AreEqual(testOwner, testSavingsAcct.AcctOwner,"Account Owner not stored correctly");
        Assert.AreEqual(testStartingBalance, testSavingsAcct.GetBalance(),"Start balance not stored correctly");
        Assert.AreEqual(testAnnualInterestRate, testSavingsAcct.GetAnnualInterestRate(),"Annual interest rate not stored correctly");
    }
    [TestMethod]
    public void SavingsAccount_Created_With_Only_Owner_Provided() {

        // Arrange
        SavingsAccount testSavingsAcct;
        string testOwner              = "Tony Stark";
        double testStartingBalance    = SavingsAccount.SavingsDefaultBalance;
        double testAnnualInterestRate = SavingsAccount.SavingsDefaultApr;

        // Act
        testSavingsAcct = new SavingsAccount(testOwner);

        // Assert
        Assert.AreEqual(testOwner, testSavingsAcct.AcctOwner,"Account Owner not stored correctly");
        Assert.AreEqual(testStartingBalance, testSavingsAcct.GetBalance(),"Start balance not stored correctly");
        Assert.AreEqual(testAnnualInterestRate, testSavingsAcct.GetAnnualInterestRate(),"Annual interest rate not stored correctly");
    }

    [TestMethod]
    public void Deposit_Is_Applied_Correctly()  {
        //Arrange
        SavingsAccount testSavingsAcct = InstantiateDefaultTestSavingsAccount();
        double originalBalance = testSavingsAcct.GetBalance();
        double testDeposit = 50;

        // Act
        testSavingsAcct.Deposit(testDeposit);

        //Assert
        Assert.AreEqual(originalBalance + testDeposit, testSavingsAcct.GetBalance(), "Deposit not stored correctly");
    }

    [TestMethod]
    [ExpectedException(typeof(InvalidDepositAmountException))]
    public void Negative_Deposit_Amount_Throws_Exception()  {
        //Arrange
        SavingsAccount testSavingsAcct = InstantiateDefaultTestSavingsAccount();
        double testDeposit = -1;

        // Act and Assert
        testSavingsAcct.Deposit(testDeposit);
    }

    [TestMethod]
    public void Withdrawal_Is_Applied_Correctly()  {
        //Arrange
        SavingsAccount testSavingsAcct = InstantiateDefaultTestSavingsAccount();
        testSavingsAcct.SetBalance(100);
        double originalBalance = testSavingsAcct.GetBalance();
        double testWithdrawal = 30;

        // Act
        testSavingsAcct.Withdraw(testWithdrawal);

        //Assert
        Assert.AreEqual(originalBalance - testWithdrawal, testSavingsAcct.GetBalance(), "Withdrawal not performed correctly");
    }

    [TestMethod]
    [ExpectedException(typeof(InvalidWithdrawAmountException))]
    public void Negative_Withdrawal_Amount_Throws_Exception() {
        //Arrange
        SavingsAccount testSavingsAcct = InstantiateDefaultTestSavingsAccount();
        double testWithdrawal = -1;

        // Act and Assert
        testSavingsAcct.Withdraw(testWithdrawal);
    }

    [TestMethod]
    public void Monthly_Interest_Is_Credited_Correctly() {

        // Arrange
        SavingsAccount testSavingsAcct = InstantiateDefaultTestSavingsAccount();
        testSavingsAcct.SetBalance(100);
        double expectedBalanceAfterInterest = testSavingsAcct.GetBalance() + (testSavingsAcct.GetBalance() * (testSavingsAcct.GetAnnualInterestRate() / 12));

        // Act
        testSavingsAcct.ApplyMonthlyInterest();

        // Assert
        Assert.AreEqual(expectedBalanceAfterInterest, testSavingsAcct.GetBalance(), .001,"Monthly interest has not been applied correctly");
    }

    private SavingsAccount InstantiateDefaultTestSavingsAccount()  {
        string defaultOwner              = "Millburn Pennybags";
        double defaultStartingBalance    = SavingsAccount.SavingsDefaultBalance;
        double defaultInterestRate       = SavingsAccount.SavingsDefaultApr;

        return  new SavingsAccount(defaultOwner,defaultStartingBalance, defaultInterestRate);
    }


}
