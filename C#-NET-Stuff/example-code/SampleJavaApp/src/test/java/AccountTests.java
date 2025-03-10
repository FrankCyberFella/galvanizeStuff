import org.example.exception.InvalidDepositAmountException;
import org.example.exception.UnableToAssignAccountNumberException;
import org.example.model.Account;
import org.example.model.CheckingAccount;
import org.junit.jupiter.api.Test;
import org.mockito.Mockito;

import java.nio.file.StandardWatchEventKinds;

import static org.junit.jupiter.api.Assertions.assertThrows;

public class AccountTests {

    @Test
    public void ExceptionThrownIfAccountIsNotAbleToGenerateUniqueAccountNumber() throws UnableToAssignAccountNumberException {

        // Arrange
        CheckingAccount anAccount = null;

        // Use all possible account numbers
        for (int i = Account.STARTING_ACCOUNT_NUMBER; i < Account.LARGEST_ACCOUNT_NUMBER; i++) {
            anAccount = new CheckingAccount("test", 0);
        }

        // Use mockito to test concrete method in abstract class
        // to attempt to create one mor account after all account numbers have been used
        Account absCls = Mockito.mock(
                Account.class,
                Mockito.CALLS_REAL_METHODS);

        // Act and Assert
        UnableToAssignAccountNumberException theException =
                assertThrows(UnableToAssignAccountNumberException.class, () -> absCls.getUniqueAcctNumber());
    }
}

