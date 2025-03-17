using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace SampleCSharpAppTests.Model
{
    public interface IBaseAccount
    {
        double GetBalance();
        double Withdraw(double amount);
        double Deposit(double amount);
    }

}
