using SampleCSharpApp.Exception;

namespace SampleCSharpApp.Model;

public class SavingsAccount : Account
    {
        public const double SAVINGS_DEFAULT_BALANCE = 0;
        public const double SAVINGS_DEFAULT_APR     = 0.01;

        private double annualInterestRate;

        public SavingsAccount(string acctOwner, double balance, double annualInterestRate)
            : base(acctOwner, balance)
        {
            this.annualInterestRate = annualInterestRate;
        }

        public SavingsAccount(string acctOwner, double balance)
            : base(acctOwner, balance)
        {
            this.annualInterestRate = SAVINGS_DEFAULT_APR;
        }

        public SavingsAccount(string acctOwner)
            : base(acctOwner, SAVINGS_DEFAULT_BALANCE)
        {
            this.annualInterestRate = SAVINGS_DEFAULT_APR;
        }

        public double GetAnnualInterestRate()
        {
            return annualInterestRate;
        }

        public void SetAnnualInterestRate(double annualInterestRate)
        {
            this.annualInterestRate = annualInterestRate;
        }

        public override double Withdraw(double amount)
        {
            if (amount > balance || amount < 0)
            {
                throw new InvalidWithdrawAmountException("Withdrawal amount (" + amount + ") exceeds balance (" + balance + ") or is less than 0");
            }
            return balance -= amount;
        }

        public override double Deposit(double amount)
        {
            if (amount < 0)
            {
                throw new InvalidDepositAmountException("Withdrawal amount (" + amount + ") cannot be less than 0");
            }
            return balance += amount;
        }

        public double ApplyMonthlyInterest()
        {
            return balance += balance * (annualInterestRate / 12);
        }

        public override string ToString()
        {
            return base.ToString() + "; \nSavingsAccount{" +
                   "annualInterestRate=" + annualInterestRate +
                   '}';
        }
    
}
