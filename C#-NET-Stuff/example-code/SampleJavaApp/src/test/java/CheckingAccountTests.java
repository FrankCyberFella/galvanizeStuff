import org.example.exception.ExcessiveWithdrawAmountException;
import org.example.exception.InvalidDepositAmountException;
import org.example.exception.InvalidWithdrawAmountException;
import org.example.exception.UnableToAssignAccountNumberException;
import org.example.model.CheckingAccount;
import org.junit.jupiter.api.Test;

import java.util.concurrent.Callable;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;

public class CheckingAccountTests {

    @Test
    public void CheckingAccount_Created_With_All_Initial_Values_Provided() throws UnableToAssignAccountNumberException {
        //Arrange
        CheckingAccount testCheckingAcct;
        String testOwner              = "Tony Stark";
        double testStartingBalance    = 100.00;
        double testOverDraftFee       = 13;
        double testMaxOverDraftAmount = 250;

        // Act
        testCheckingAcct = new  CheckingAccount(testOwner, testStartingBalance, testOverDraftFee, testMaxOverDraftAmount);

        //Assert
        assertEquals(testOwner, testCheckingAcct.getAcctOwner(),"Account Owner not stored correctly");
        assertEquals(testStartingBalance, testCheckingAcct.getBalance(),"Start balance not stored correctly");
        assertEquals(testOverDraftFee, testCheckingAcct.getOverdraftFee(),"Overdraft fee  not stored correctly");
        assertEquals(testMaxOverDraftAmount, testCheckingAcct.getMaxOverdraftAmount(),"Max overdraft amount not stored correctly");

    }

    @Test
    public void CheckingAccount_Created_With_Owner_StartBalance_Overdraft_Fee_Provided() throws UnableToAssignAccountNumberException {
        //Arrange
        CheckingAccount testCheckingAcct;
        String testOwner              = "Richie Rich";
        double testStartingBalance    = 100.00;
        double testOverDraftFee       = 15;

        // Act
        testCheckingAcct = new  CheckingAccount(testOwner, testStartingBalance, testOverDraftFee);

        //Assert
        assertEquals(testOwner, testCheckingAcct.getAcctOwner(),"Account Owner not stored correctly");
        assertEquals(testStartingBalance, testCheckingAcct.getBalance(),"Start balance not stored correctly");
        assertEquals(testOverDraftFee, testCheckingAcct.getOverdraftFee(),"Overdraft fee  not stored correctly");
        assertEquals(CheckingAccount.STANDARD_MAX_OVERDRAFT_AMOUNT, testCheckingAcct.getMaxOverdraftAmount(),"Max overdraft amount not stored correctly");

    }
    @Test
    public void CheckingAccount_Created_With_Owner_Start_Balance_Provided() throws UnableToAssignAccountNumberException {
        //Arrange
        CheckingAccount testCheckingAcct;
        String testOwner           = "Scrooge McDuck";
        double testStartingBalance = 100.00;

        // Act
        testCheckingAcct = new  CheckingAccount(testOwner, testStartingBalance);

        //Assert
        assertEquals(testOwner, testCheckingAcct.getAcctOwner(),"Account Owner not stored correctly");
        assertEquals(testStartingBalance, testCheckingAcct.getBalance(),"Start balance not stored correctly");
        assertEquals(CheckingAccount.STANDARD_OVERDRAFT_FEE, testCheckingAcct.getOverdraftFee(),"Overdraft fee  not stored correctly");
        assertEquals(CheckingAccount.STANDARD_MAX_OVERDRAFT_AMOUNT, testCheckingAcct.getMaxOverdraftAmount(),"Max overdraft amount not stored correctly");
    }
    @Test
    public void Deposit_Is_Applied_Correctly() throws UnableToAssignAccountNumberException {
        //Arrange
        CheckingAccount testCheckingAcct = InstantiateDefaultTestCheckingAccount();
        double originalBalance = testCheckingAcct.getBalance();
        double testDeposit = 50;

        // Act
        testCheckingAcct.deposit(testDeposit);

        //Assert
        assertEquals(originalBalance + testDeposit, testCheckingAcct.getBalance(), "Deposit not stored correctly");
    }

    @Test
    public void Negative_Deposit_Amount_Throws_Exception() throws UnableToAssignAccountNumberException {
        //Arrange
        CheckingAccount testCheckingAcct = InstantiateDefaultTestCheckingAccount();
        double testDeposit = -1;

        // Act and Assert
        InvalidDepositAmountException thrownException = assertThrows(InvalidDepositAmountException.class, () -> testCheckingAcct.deposit(-1));
    }
    @Test
    public void Withdrawal_Is_Applied_Correctly() throws UnableToAssignAccountNumberException {
      // Arrange
        double testStartingBalance = 100;
        double testWithdrawlAmount = 10;

        CheckingAccount testCheckingAccount = InstantiateDefaultTestCheckingAccount();

        testCheckingAccount.setBalance(testStartingBalance);

      // Act
        testCheckingAccount.withdraw(testWithdrawlAmount);

      // Assert
        assertEquals(testStartingBalance-testWithdrawlAmount, testCheckingAccount.getBalance(),"Withdrawal amount not applied correctly");
    }

    @Test
    public void Overdraft_Withdrawal_Less_Than_Or_Equal_Max_Overdraft_Allowed_After_Successfully_Charging_Late_fee() throws UnableToAssignAccountNumberException {
        // Arrange
        double testStartingBalance = 0;
        double testWithdrawlAmount = CheckingAccount.STANDARD_MAX_OVERDRAFT_AMOUNT - CheckingAccount.STANDARD_OVERDRAFT_FEE;
        double testEndingBalance   = testStartingBalance;

        CheckingAccount testCheckingAccount = InstantiateDefaultTestCheckingAccount();
        testCheckingAccount.setBalance(testStartingBalance);

        // Act and Assert - Exception check
        ExcessiveWithdrawAmountException thrownException = assertThrows(ExcessiveWithdrawAmountException.class, () -> testCheckingAccount.withdraw(testWithdrawlAmount+1));

        // Assert - Balance unchanged check
        assertEquals(testEndingBalance, testCheckingAccount.getBalance(),"Withdrawal should not have changed balance due to excessive overdraft attempted");
    }

    @Test
    public void Negative_Withdrawal_Amount_Throws_Expected_Eception() throws UnableToAssignAccountNumberException {
        // Arrange
        CheckingAccount testCheckingAccount = InstantiateDefaultTestCheckingAccount();
        double expectedBalance = testCheckingAccount.getBalance();

        // Act and Assert - Exception check
        InvalidWithdrawAmountException thrownException = assertThrows(InvalidWithdrawAmountException.class, () -> testCheckingAccount.withdraw(-1));
    }


    
    private CheckingAccount InstantiateDefaultTestCheckingAccount() throws UnableToAssignAccountNumberException {
        String defaultOwner              = "Millburn Pennybags";
        double defaultStartingBalance    = CheckingAccount.STANDARD_START_BALANCE;
        double defaultOverDraftFee       = CheckingAccount.STANDARD_OVERDRAFT_FEE;
        double defaultMaxOverDraftAmount = CheckingAccount.STANDARD_MAX_OVERDRAFT_AMOUNT;

        return  new CheckingAccount(defaultOwner,defaultStartingBalance, defaultOverDraftFee, defaultMaxOverDraftAmount);
     }
}
