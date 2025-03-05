using Day_1_Unit_Testing;
using System.Data.Common;
using System.Diagnostics.Metrics;
using System.Transactions;
using Xunit;

namespace CheckingAccountTests
{
    public class CheckingAcountTests
    {
        /**********************************************************************************
         * Generate Unit tests for "Happy Path" and "Edge/Error" cases
         * 
         * Quite often, the requirements for the appliction has great starting points
         * for tests.
         * 
         * For this class we were given:
         * 
         *    An account has a balance of 0 and attempts a withdrawal of $90
         *    ALLOWED as the balance after the transaction would be -100 (with overdraft fee)
         *
         *    An account has a balance of 0 and attempts a withdrawal of $100
         *    NOT ALLOWED as the balance after the transaction would be -110 (with overdraft fee)
         * 
         *    An account has a balance of 100 and attempts a withdrawal of $100
         *    ALLOWED as the balance after the transaction would be 0
         *********************************************************************************/

        /*    An account has a balance of 0 and attempts a withdrawal of $90
         *    ALLOWED as the balance after the transaction would be -100 (with overdraft fee)
         **/

        [Fact]
        public void Withdrawl_With_Overdraft_Fee()
        {
            // Arrange - define test data for test

            double testStartBalance = 0; // Starting Balance

            // CheckingAccount with a known balance
            CheckingAccount testAccount = new CheckingAccount("test Owner", testStartBalance);

            // Withdrawal amount greater than the balance (but less than $10 more than balance)
            double testWithdrawal = 90;

            // Act - test method using test data
            testAccount.Withdraw(testWithdrawal);

            // Assert - verify we get the expected value
            //          is the balance -100
            Assert.Equal(-100, testAccount.Balance);
        } // End of a Unit test

         /*    An account has a balance of 0 and attempts a withdrawal of $100
         *    NOT ALLOWED as the balance after the transaction would be -110 (with overdraft fee) */

        [Fact]
        public void Withdrawal_Over_Allowed_Overdraft_Amount()
        {
            // Arrange - setup test data
            double startBalance = 0;

            CheckingAccount testAccount = new CheckingAccount("Ryan", startBalance);

            // Act - run the process with test data
            testAccount.Withdraw(100);

            // Assert - Test results for validity
            // Since the withdraw should not be allowed, the startBalance should be unchanged
            Assert.Equal(startBalance, testAccount.Balance);
        }

        /*    An account has a balance of 100 and attempts a withdrawal of $100
         *    ALLOWED as the balance after the transaction would be 0           */

        [Fact]
        public void Withdraw_Of_Allowed_Amount()
        {
            // Arrange - Define test data

            double startBalance = 100;

            CheckingAccount testAccount = new CheckingAccount("Jay", startBalance);

            // Act - Run the process with the test data

            testAccount.Withdraw(100);

            // Assert

            Assert.Equal(0, testAccount.Balance);
        }
    } // End of the class holding the unit tests
} // End of namespaces