namespace SampleCSharpApp.Model;
    public interface IBaseAccount
    {
        double GetBalance();
        double Withdraw(double amount);
        double Deposit(double amount);
    }

