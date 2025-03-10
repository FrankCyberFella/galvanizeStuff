using Microsoft.VisualStudio.TestTools.UnitTesting;
using SampleUnitTests.Exception;
using SampleUnitTests.Model;

namespace SampleUnitTests;

[TestClass]
public class CheckingAccountTests
{

    [TestMethod]
    public void CheckingAccount_Created_With_All_Initial_Values_Provided()
    {
        //Arrange
        CheckingAccount testCheckingAcct;
        string testOwner = "Tony Stark";
        double testStartingBalance = 100.00;
        double testOverDraftFee = 13;
        double testMaxOverDraftAmount = 250;

        // Act
        testCheckingAcct =
            new CheckingAccount(testOwner, testStartingBalance, testOverDraftFee, testMaxOverDraftAmount);

        //Assert

        Assert.AreEqual(testOwner, testCheckingAcct.AcctOwner, "Account Owner not stored correctly");
        Assert.AreEqual(testStartingBalance, testCheckingAcct.GetBalance(), "Start balance not stored correctly");
        Assert.AreEqual(testOverDraftFee, testCheckingAcct.OverdraftFee, "Overdraft fee  not stored correctly");
        Assert.AreEqual(testMaxOverDraftAmount, testCheckingAcct.MaxOverdraftAmount,
            "Max overdraft amount not stored correctly");

    }

    [TestMethod]
    public void CheckingAccount_Created_With_Owner_StartBalance_Overdraft_Fee_Provided()
    {
        //Arrange
        CheckingAccount testCheckingAcct;
        string testOwner = "Richie Rich";
        double testStartingBalance = 100.00;
        double testOverDraftFee = 15;

        // Act
        testCheckingAcct = new CheckingAccount(testOwner, testStartingBalance, testOverDraftFee);

        //Assert
        Assert.AreEqual(testOwner, testCheckingAcct.AcctOwner, "Account Owner not stored correctly");
        Assert.AreEqual(testStartingBalance, testCheckingAcct.GetBalance(), "Start balance not stored correctly");
        Assert.AreEqual(testOverDraftFee, testCheckingAcct.OverdraftFee, "Overdraft fee  not stored correctly");
        Assert.AreEqual(CheckingAccount.StandardMaxOverdraftAmount, testCheckingAcct.MaxOverdraftAmount,
            "Max overdraft amount not stored correctly");

    }

    [TestMethod]
    public void CheckingAccount_Created_With_Owner_Start_Balance_Provided()
    {
        //Arrange
        CheckingAccount testCheckingAcct;
        string testOwner = "Scrooge McDuck";
        double testStartingBalance = 100.00;

        // Act
        testCheckingAcct = new CheckingAccount(testOwner, testStartingBalance);

        //Assert
        Assert.AreEqual(testOwner, testCheckingAcct.AcctOwner, "Account Owner not stored correctly");
        Assert.AreEqual(testStartingBalance, testCheckingAcct.GetBalance(), "Start balance not stored correctly");
        Assert.AreEqual(CheckingAccount.StandardOverdraftFee, testCheckingAcct.OverdraftFee,
            "Overdraft fee  not stored correctly");
        Assert.AreEqual(CheckingAccount.StandardMaxOverdraftAmount, testCheckingAcct.MaxOverdraftAmount,
            "Max overdraft amount not stored correctly");
    }

    [TestMethod]
    public void Deposit_Is_Applied_Correctly()
    {
        //Arrange
        CheckingAccount testCheckingAcct = InstantiateDefaultTestCheckingAccount();
        double originalBalance = testCheckingAcct.GetBalance();
        double testDeposit = 50;

        // Act
        testCheckingAcct.Deposit(testDeposit);

        //Assert
        Assert.AreEqual(originalBalance + testDeposit, testCheckingAcct.GetBalance(), "Deposit not stored correctly");
    }

    [TestMethod]
    [ExpectedException(typeof(InvalidDepositAmountException))]
    public void Negative_Deposit_Amount_Throws_Exception()
    {
        //Arrange
        CheckingAccount testCheckingAcct = InstantiateDefaultTestCheckingAccount();
        double testDeposit = -1;

        // Act and Assert
        testCheckingAcct.Deposit(-1);
    }

    [TestMethod]
    public void Withdrawal_Is_Applied_Correctly()
    {
        // Arrange
        double testStartingBalance = 100;
        double testWithdrawlAmount = 10;

        CheckingAccount testCheckingAccount = InstantiateDefaultTestCheckingAccount();

        testCheckingAccount.SetBalance(testStartingBalance);

        // Act
        testCheckingAccount.Withdraw(testWithdrawlAmount);

        // Assert
        Assert.AreEqual(testStartingBalance - testWithdrawlAmount, testCheckingAccount.GetBalance(),
            "Withdrawal amount not applied correctly");
    }

    [TestMethod]
    [ExpectedException(typeof(ExcessiveWithdrawAmountException))]
    public void Overdraft_Withdrawal_Less_Than_Or_Equal_Max_Overdraft_Allowed_After_Successfully_Charging_Late_fee()
    {
        // Arrange
        double testStartingBalance = 0;
        double testWithdrawlAmount =
            CheckingAccount.StandardMaxOverdraftAmount - CheckingAccount.StandardOverdraftFee;
        double testEndingBalance = testStartingBalance;

        CheckingAccount testCheckingAccount = InstantiateDefaultTestCheckingAccount();
        testCheckingAccount.SetBalance(testStartingBalance);

        // Act and Assert - Exception check
        testCheckingAccount.Withdraw(testWithdrawlAmount + 1);

        // Assert - Balance unchanged check
        Assert.AreEqual(testEndingBalance, testCheckingAccount.GetBalance(),
            "Withdrawal should not have changed balance due to excessive overdraft attempted");
    }

    [TestMethod]
    [ExpectedException(typeof(InvalidWithdrawAmountException))]
    public void Negative_Withdrawal_Amount_Throws_Expected_Eception()
    {
        // Arrange
        CheckingAccount testCheckingAccount = InstantiateDefaultTestCheckingAccount();
        double expectedBalance = testCheckingAccount.GetBalance();

        // Act and Assert - Exception check
        testCheckingAccount.Withdraw(-1);
    }


    private CheckingAccount InstantiateDefaultTestCheckingAccount()
    {
        string defaultOwner = "Millburn Pennybags";
        double defaultStartingBalance = CheckingAccount.StandardStartBalance;
        double defaultOverDraftFee = CheckingAccount.StandardOverdraftFee;
        double defaultMaxOverDraftAmount = CheckingAccount.StandardMaxOverdraftAmount;

        return new CheckingAccount(defaultOwner, defaultStartingBalance, defaultOverDraftFee,
            defaultMaxOverDraftAmount);
    }
}
