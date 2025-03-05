using System.Collections.Generic;

namespace Exercises
{
    public partial class Exercises
    {
        /*
         * Modify and return the given Dictionary as follows: if "Peter" has more than 0 money, transfer half of it to "Paul",
         * but only if Paul has less than $10.
         *
         * Note, monetary amounts are specified in cents: penny=1, nickel=5, ... $1=100, ... $10=1000, ...
         *
         * RobPeterToPayPaul({"Peter": 2000, "Paul": 99}) → {"Peter": 1000, "Paul": 1099}
         * RobPeterToPayPaul({"Peter": 2000, "Paul": 30000}) → {"Peter": 2000, "Paul": 30000}
         * RobPeterToPayPaul({"Peter": 101, "Paul": 500}) → {"Peter": 51, "Paul": 550}
         * RobPeterToPayPaul({"Peter": 0, "Paul": 500}) → {"Peter": 0, "Paul": 500}
         *
         */
        public Dictionary<string, int> RobPeterToPayPaul(Dictionary<string, int> peterPaul)
        {
            int petersMoney = peterPaul["Peter"];
            int paulsMoney = peterPaul["Paul"];

            if (petersMoney > 0 && paulsMoney < 1000)
            {
                int moneyToPayPaul = petersMoney / 2;
                peterPaul["Paul"] = moneyToPayPaul + peterPaul["Paul"];
                peterPaul["Peter"] = petersMoney - moneyToPayPaul;
            }

            return peterPaul;
        }
    }
}
