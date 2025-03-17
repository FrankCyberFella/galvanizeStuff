
using SampleCSharpAppTests.Exception;
using System.Globalization;

namespace SampleCSharpAppTests.Model
{
    public class CheckingAccount : Account
    {
        public const double StandardStartBalance = 0;
        public const double StandardOverdraftFee = 10.00;
        public const double StandardMaxOverdraftAmount = 100.00;

        public double OverdraftFee { get; set; }
        public double MaxOverdraftAmount { get; set; }

        public CheckingAccount(string acctOwner, double balance, double overdraftFee, double maxOverdraftAmount) : base(acctOwner, balance)
        {
            this.OverdraftFee = overdraftFee;
            this.MaxOverdraftAmount = maxOverdraftAmount;
        }
        public CheckingAccount(string acctOwner, double balance, double overdraftFee) : base(acctOwner, balance)
        {
            this.OverdraftFee = overdraftFee;
            this.MaxOverdraftAmount = StandardMaxOverdraftAmount;
        }
        public CheckingAccount(string acctOwner, double balance) : base(acctOwner, balance)
        {
            this.OverdraftFee = StandardOverdraftFee;
            this.MaxOverdraftAmount = StandardMaxOverdraftAmount;
        }
        public override double Withdraw(double amount)
        {
            if (amount < 0)
            {
                throw new InvalidWithdrawAmountException("Deposit amount cannot be less that zero");
            }

            double pendingBalance = GetBalance() - amount;
            pendingBalance -= ((pendingBalance < 0) && (pendingBalance <= MaxOverdraftAmount)) ? OverdraftFee : 0;

            if (pendingBalance >= -MaxOverdraftAmount)
            {
                SetBalance(pendingBalance);
            }
            else
            {
                // In C#, instead of Java's DecimalFormat, we format numbers using ToString with a custom format and CultureInfo.
                string formattedAmount = amount.ToString("$###,###.##", CultureInfo.InvariantCulture);
                string formattedOverFee = OverdraftFee.ToString("$###,###.##", CultureInfo.InvariantCulture);
                string formattedBalance = GetBalance().ToString("$###,###.##", CultureInfo.InvariantCulture);
                string formattedOverdraft = MaxOverdraftAmount.ToString("$###,###.##", CultureInfo.InvariantCulture);

                string overdrawMessage = string.Format("Withdrawal ({0}) with overdraft fee ({1}) from Balance ({2}) would have exceeded allowable overdraft amount ({3})"
                                                      , formattedAmount
                                                      , formattedOverFee
                                                      , formattedBalance
                                                      , formattedOverdraft);

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
                   "overdraftFee=" + OverdraftFee +
                   ", maxOverdraftAmount=" + MaxOverdraftAmount +
                   '}';
        }

    }
}
