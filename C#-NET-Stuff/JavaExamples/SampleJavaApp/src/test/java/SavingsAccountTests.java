import org.example.exception.InvalidDepositAmountException;
import org.example.exception.InvalidWithdrawAmountException;
import org.example.exception.UnableToAssignAccountNumberException;
import org.example.model.CheckingAccount;
import org.example.model.SavingsAccount;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;

public class SavingsAccountTests {

    @Test
    public void SavingsAccount_Created_With_All_Initial_Values_Provided() throws UnableToAssignAccountNumberException {

        // Arrange
        SavingsAccount testSavingsAcct;
        String testOwner              = "Tony Stark";
        double testStartingBalance    = 100.00;
        double testAnnualInterestRate = .03;

        // Act
        testSavingsAcct = new SavingsAccount(testOwner, testStartingBalance, testAnnualInterestRate);

        // Assert
        Assertions.assertEquals(testOwner, testSavingsAcct.getAcctOwner(),"Account Owner not stored correctly");
        Assertions.assertEquals(testStartingBalance, testSavingsAcct.getBalance(),"Start balance not stored correctly");
        Assertions.assertEquals(testAnnualInterestRate, testSavingsAcct.getAnnualInterestRate(),"Annual interest rate not stored correctly");
        }

@Test
    public void SavingsAccount_Created_With_Only_Owner_And_Balance_Provided() throws UnableToAssignAccountNumberException {

        // Arrange
        SavingsAccount testSavingsAcct;
        String testOwner              = "Tony Stark";
        double testStartingBalance    = 100.00;
        double testAnnualInterestRate = SavingsAccount.SAVINGS_DEFAULT_APR;

        // Act
        testSavingsAcct = new SavingsAccount(testOwner, testStartingBalance);

        // Assert
        Assertions.assertEquals(testOwner, testSavingsAcct.getAcctOwner(),"Account Owner not stored correctly");
        Assertions.assertEquals(testStartingBalance, testSavingsAcct.getBalance(),"Start balance not stored correctly");
        Assertions.assertEquals(testAnnualInterestRate, testSavingsAcct.getAnnualInterestRate(),"Annual interest rate not stored correctly");
    }
    @Test
    public void SavingsAccount_Created_With_Only_Owner_Provided() throws UnableToAssignAccountNumberException {

        // Arrange
        SavingsAccount testSavingsAcct;
        String testOwner              = "Tony Stark";
        double testStartingBalance    = SavingsAccount.SAVINGS_DEFAULT_BALANCE;
        double testAnnualInterestRate = SavingsAccount.SAVINGS_DEFAULT_APR;

        // Act
        testSavingsAcct = new SavingsAccount(testOwner);

        // Assert
        Assertions.assertEquals(testOwner, testSavingsAcct.getAcctOwner(),"Account Owner not stored correctly");
        Assertions.assertEquals(testStartingBalance, testSavingsAcct.getBalance(),"Start balance not stored correctly");
        Assertions.assertEquals(testAnnualInterestRate, testSavingsAcct.getAnnualInterestRate(),"Annual interest rate not stored correctly");
    }

    @Test
    public void Deposit_Is_Applied_Correctly() throws UnableToAssignAccountNumberException {
        //Arrange
        SavingsAccount testSavingsAcct = InstantiateDefaultTestSavingsAccount();
        double originalBalance = testSavingsAcct.getBalance();
        double testDeposit = 50;

        // Act
        testSavingsAcct.deposit(testDeposit);

        //Assert
        assertEquals(originalBalance + testDeposit, testSavingsAcct.getBalance(), "Deposit not stored correctly");
    }

    @Test
    public void Negative_Deposit_Amount_Throws_Exception() throws UnableToAssignAccountNumberException {
        //Arrange
        SavingsAccount testSavingsAcct = InstantiateDefaultTestSavingsAccount();
        double testDeposit = -1;

        // Act and Assert
        InvalidDepositAmountException thrownException = assertThrows(InvalidDepositAmountException.class, () -> testSavingsAcct.deposit(-1));
    }

@Test
public void Withdrawal_Is_Applied_Correctly() throws UnableToAssignAccountNumberException {
    //Arrange
    SavingsAccount testSavingsAcct = InstantiateDefaultTestSavingsAccount();
    testSavingsAcct.setBalance(100);
    double originalBalance = testSavingsAcct.getBalance();
    double testWithdrawal = 30;

    // Act
    testSavingsAcct.withdraw(testWithdrawal);

    //Assert
    assertEquals(originalBalance - testWithdrawal, testSavingsAcct.getBalance(), "Withdrawal not performed correctly");
}

    @Test
    public void Negative_Withdrawal_Amount_Throws_Exception() throws UnableToAssignAccountNumberException {
        //Arrange
        SavingsAccount testSavingsAcct = InstantiateDefaultTestSavingsAccount();
        double testWithdrawal = -1;

        // Act and Assert
        InvalidWithdrawAmountException thrownException = assertThrows(InvalidWithdrawAmountException.class, () -> testSavingsAcct.withdraw(-1));
    }

    @Test
    public void Monthly_Interest_Is_Credited_Correctly() throws UnableToAssignAccountNumberException {

        // Arrange
        SavingsAccount testSavingsAcct = InstantiateDefaultTestSavingsAccount();
        testSavingsAcct.setBalance(100);
        double expectedBalanceAfterInterest = testSavingsAcct.getBalance() + (testSavingsAcct.getBalance() * (testSavingsAcct.getAnnualInterestRate() / 12));

        // Act
        testSavingsAcct.applyMonthlyInterest();

        // Assert
        Assertions.assertEquals(expectedBalanceAfterInterest, testSavingsAcct.getBalance(), .001,"Monthly interest has not been applied correctly");
    }

    private SavingsAccount InstantiateDefaultTestSavingsAccount() throws UnableToAssignAccountNumberException {
        String defaultOwner              = "Millburn Pennybags";
        double defaultStartingBalance    = SavingsAccount.SAVINGS_DEFAULT_BALANCE;
        double defaultInterestRate       = SavingsAccount.SAVINGS_DEFAULT_APR;

        return  new SavingsAccount(defaultOwner,defaultStartingBalance, defaultInterestRate);
    }


}
