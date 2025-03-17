using System;
using System.Globalization;
using SampleCSharpApp.Exception;

namespace SampleCSharpApp.Model;

    public class CheckingAccount : Account
    {
        public const double STANDARD_START_BALANCE        = 0;
        public const double STANDARD_OVERDRAFT_FEE        = 10.00;
        public const double STANDARD_MAX_OVERDRAFT_AMOUNT = 100.00;

        private double overdraftFee;
        private double maxOverdraftAmount;

        public CheckingAccount(string acctOwner, double balance, double overdraftFee, double maxOverdraftAmount) : base(acctOwner, balance)
        {
            this.overdraftFee       = overdraftFee;
            this.maxOverdraftAmount = maxOverdraftAmount;
        }
        public CheckingAccount(string acctOwner, double balance, double overdraftFee) : base(acctOwner, balance)
        {
            this.overdraftFee       = overdraftFee;
            this.maxOverdraftAmount = STANDARD_MAX_OVERDRAFT_AMOUNT;
        }
        public CheckingAccount(string acctOwner, double balance) : base(acctOwner, balance)
        {
            this.overdraftFee       = STANDARD_OVERDRAFT_FEE;
            this.maxOverdraftAmount = STANDARD_MAX_OVERDRAFT_AMOUNT;
        }

        public double GetOverdraftFee()
        {
            return overdraftFee;
        }

        public void SetOverdraftFee(double overdraftFee)
        {
            this.overdraftFee = overdraftFee;
        }

        public double GetMaxOverdraftAmount()
        {
            return maxOverdraftAmount;
        }

        public void SetMaxOverdraftAmount(double maxOverdraftAmount)
        {
            this.maxOverdraftAmount = maxOverdraftAmount;
        }

        public override double Withdraw(double amount)
        {

            if (amount < 0)
            {
                throw new InvalidWithdrawAmountException("Deposit amount cannot be less that zero");
            }

            double pendingBalance = GetBalance() - amount;
            pendingBalance -= ((pendingBalance < 0) && (pendingBalance <= maxOverdraftAmount)) ? overdraftFee : 0;

            if (pendingBalance >= -maxOverdraftAmount)
            {
                SetBalance(pendingBalance);
            }
            else
            {
                // In C#, instead of Java's DecimalFormat, we format numbers using ToString with a custom format and CultureInfo.
                string formattedAmount     = amount.ToString("$###,###.##", CultureInfo.InvariantCulture);
                string formattedOverFee    = overdraftFee.ToString("$###,###.##", CultureInfo.InvariantCulture);
                string formattedBalance    = GetBalance().ToString("$###,###.##", CultureInfo.InvariantCulture);
                string formattedOverdraft  = maxOverdraftAmount.ToString("$###,###.##", CultureInfo.InvariantCulture);

                string overdrawMessage = string.Format("Withdrawal ({0}) with overdraft fee ({1}) from Balance ({2}) would have exceeded allowable overdraft amount ({3})"
                                                      ,formattedAmount
                                                      ,formattedOverFee
                                                      ,formattedBalance
                                                      ,formattedOverdraft);

                throw new ExcessiveWithdrawAmountException(overdrawMessage);
            }

            return GetBalance();
        }

        public override double Deposit(double amount)
        {
            if (amount < 0)
            {
                throw new InvalidDepositAmountException("Deposit amount cannot be less that zero");
            }
            SetBalance(GetBalance() + amount);
            return GetBalance();
        }

        public override string ToString()
        {
            return base.ToString() + ";\nCheckingAccount{" +
                   "overdraftFee=" + overdraftFee +
                   ", maxOverdraftAmount=" + maxOverdraftAmount +
                   '}';
        }
    
}
