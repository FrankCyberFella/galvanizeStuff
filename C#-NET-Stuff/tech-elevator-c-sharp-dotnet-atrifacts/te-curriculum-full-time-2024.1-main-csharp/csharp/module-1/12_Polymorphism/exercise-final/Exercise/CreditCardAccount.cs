namespace Exercise
{
    public class CreditCardAccount : IAccountable
    {
        public decimal Debt { get; private set; }
        public string AccountHolderName { get; private set; }
        public string CardNumber { get; }
        public decimal Balance
        {
            get { return -Debt; }
        }

        public CreditCardAccount(string accountHolderName, string cardNumber)
        {
            AccountHolderName = accountHolderName;
            CardNumber = cardNumber;
        }

        public decimal Pay(decimal amountToPay)
        {
            if (amountToPay > 0)
            {
                Debt -= amountToPay;
            }
            return Debt;
        }

        public decimal Charge(decimal amountToCharge)
        {
            if (amountToCharge > 0)
            {
                Debt += amountToCharge;
            }
            return Debt;
        }
    }
}
