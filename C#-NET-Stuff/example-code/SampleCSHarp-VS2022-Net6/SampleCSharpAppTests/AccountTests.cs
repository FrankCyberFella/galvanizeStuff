using SampleCSharpAppTests.Model;

namespace SampleCSharpAppTests
{
    [TestClass]
    public class AccountTests
    {
        [TestMethod]
        public void ExceptionThrownIfAccountIsNotAbleToGenerateUniqueAccountNumber()
        {
            // Arrange
            CheckingAccount anAccount = null;

            // Use all possible account numbers
            for (int i = Account.StartingAccountNumber; i < Account.LargestAccountNumber; i++)
            {
                anAccount = new CheckingAccount("test", 0);
            }
            /*
                        // Use Moq to test concrete method in abstract class
                        // to attempt to create one more account after all account numbers have been used
                        var absClsMock = new Mock<Account>() { CallBase = true };
                        Account absCls = absClsMock.Object;

                        // Act and Assert
                        UnableToAssignAccountNumberException theException =
                            Assert.Throws<UnableToAssignAccountNumberException>(() => absCls.getUniqueAcctNumber());
            */
        }

    }
}

