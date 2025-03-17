using SampleCSharpAppTests.Exception;

namespace SampleCSharpAppTests.Model
{
    public class SavingsAccount : Account
    {
        public const double SavingsDefaultBalance = 0;
        public const double SavingsDefaultApr = 0.01;

        private double _annualInterestRate;

        public SavingsAccount(string acctOwner, double balance, double annualInterestRate)
            : base(acctOwner, balance)
        {
            this._annualInterestRate = annualInterestRate;
        }

        public SavingsAccount(string acctOwner, double balance)
            : base(acctOwner, balance)
        {
            this._annualInterestRate = SavingsDefaultApr;
        }

        public SavingsAccount(string acctOwner)
            : base(acctOwner, SavingsDefaultBalance)
        {
            this._annualInterestRate = SavingsDefaultApr;
        }

        public double GetAnnualInterestRate()
        {
            return _annualInterestRate;
        }

        public void SetAnnualInterestRate(double annualInterestRate)
        {
            this._annualInterestRate = annualInterestRate;
        }

        public override double Withdraw(double amount)
        {
            if (amount > Balance || amount < 0)
            {
                throw new InvalidWithdrawAmountException("Withdrawal amount (" + amount + ") exceeds balance (" + Balance + ") or is less than 0");
            }
            return Balance -= amount;
        }

        public override double Deposit(double amount)
        {
            if (amount < 0)
            {
                throw new InvalidDepositAmountException("Withdrawal amount (" + amount + ") cannot be less than 0");
            }
            return Balance += amount;
        }

        public double ApplyMonthlyInterest()
        {
            return Balance += Balance * (_annualInterestRate / 12);
        }

        public override string ToString()
        {
            return base.ToString() + "; \nSavingsAccount{" +
                   "annualInterestRate=" + _annualInterestRate +
                   '}';
        }

    }
}

