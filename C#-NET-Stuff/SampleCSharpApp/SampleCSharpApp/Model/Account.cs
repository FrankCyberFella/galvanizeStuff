
using SampleCSharpApp.Exception;

namespace SampleCSharpApp.Model;

    public abstract class Account : IBaseAccount
    {
        public const int STARTING_ACCOUNT_NUMBER = 10000;
        public const int LARGEST_ACCOUNT_NUMBER = 30000;

        private static HashSet<int> existingAccts;
        private static int maxAcctNumAttempts = 30000;

        private int acctNumber;
        private string acctOwner;
        protected double balance;

        static Account()
        {
            existingAccts = new HashSet<int>();
        }

        public Account(string acctOwner, double balance)
        {
            this.acctNumber = GetUniqueAcctNumber();
            this.acctOwner = acctOwner;
            this.balance = balance;
        }

        public int GetUniqueAcctNumber()
        {
            int newAcctNum = 0;
            Random randomizer = new Random();
            bool foundUniqueAcctNum = false;
            int loopCount = 0;
            while (!foundUniqueAcctNum && loopCount < maxAcctNumAttempts)
            {
                newAcctNum = randomizer.Next(STARTING_ACCOUNT_NUMBER, LARGEST_ACCOUNT_NUMBER);
                foundUniqueAcctNum = existingAccts.Add(newAcctNum);
                loopCount++;
            }
            if (foundUniqueAcctNum)
                return newAcctNum;
            else
                throw new UnableToAssignAccountNumberException("Unable to find unique account number - Account not created");
        }

        public override string ToString()
        {
            return $"Account{{ acctNumber={acctNumber}, acctOwner='{acctOwner}', balance={balance} }}";
        }

        public double GetBalance()
        {
            return balance;
        }

        public int GetAcctNumber()
        {
            return acctNumber;
        }

        public void SetAcctNumber(int acctNumber)
        {
            this.acctNumber = acctNumber;
        }

        public string GetAcctOwner()
        {
            return acctOwner;
        }

        public void SetAcctOwner(string acctOwner)
        {
            this.acctOwner = acctOwner;
        }

        public void SetBalance(double balance)
        {
            this.balance = balance;
        }

        public abstract double Withdraw(double amount);
        public abstract double Deposit(double amount);
    }
