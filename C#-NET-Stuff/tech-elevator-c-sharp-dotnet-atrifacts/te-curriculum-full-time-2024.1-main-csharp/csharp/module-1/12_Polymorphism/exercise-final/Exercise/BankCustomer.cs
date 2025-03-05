using System.Collections.Generic;

namespace Exercise
{
    public class BankCustomer
    {
        public string Name { get; set; }
        public string Address { get; set; }
        public string PhoneNumber { get; set; }
        private IList<IAccountable> accounts = new List<IAccountable>();

        public bool IsVip()
        {
            decimal total = 0;
            foreach (IAccountable account in accounts)
            {
                total += account.Balance;
            }
            return total >= 25000;
        }

        public IAccountable[] GetAccounts()
        {
            IAccountable[] retVal = new IAccountable[accounts.Count];
            for (int i = 0; i < accounts.Count; i++)
            {
                retVal[i] = accounts[i];
            }
            return retVal;
        }

        public void AddAccount(IAccountable newAccount)
        {
            accounts.Add(newAccount);
        }
    }
}
