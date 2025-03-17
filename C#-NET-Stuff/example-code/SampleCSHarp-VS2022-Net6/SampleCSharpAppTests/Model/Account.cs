using SampleCSharpAppTests.Exception;

namespace SampleCSharpAppTests.Model
{
    public abstract class Account : IBaseAccount
    {
        public const int StartingAccountNumber = 10000;
        public const int LargestAccountNumber = 30000;

        private static SortedSet<int> _existingAccts;
        private static int _maxAcctNumAttempts = LargestAccountNumber;

        private int _acctNumber;
        public string AcctOwner { get; set; }
        protected double Balance { get; set; }

        static Account()
        {
            _existingAccts = new SortedSet<int>();
        }
        public Account(string acctOwner, double balance)
        {
            this._acctNumber = GetUniqueAcctNumber();
            this.AcctOwner = acctOwner;
            this.Balance = balance;
        }

        public int GetUniqueAcctNumber()
        {
            int newAcctNum = 0;
            Random randomizer = new Random();
            bool foundUniqueAcctNum = false;
            int loopCount = 0;
            while (!foundUniqueAcctNum && loopCount < _maxAcctNumAttempts)
            {
                newAcctNum = randomizer.Next(StartingAccountNumber, LargestAccountNumber);
                foundUniqueAcctNum = _existingAccts.Add(newAcctNum);
                loopCount++;
            }
            if (foundUniqueAcctNum)
                return newAcctNum;
            else
                throw new UnableToAssignAccountNumberException("Unable to find unique account number - Account not created");
        }

        public override string ToString()
        {
            return $"Account{{ acctNumber={_acctNumber}, acctOwner='{AcctOwner}', balance={Balance} }}";
        }

        public double GetBalance()
        {
            return Balance;
        }
        public void SetBalance(double balance)
        {
            this.Balance = balance;
        }

        public int GetAcctNumber()
        {
            return _acctNumber;
        }
        public abstract double Withdraw(double amount);
        public abstract double Deposit(double amount);
    }
}
