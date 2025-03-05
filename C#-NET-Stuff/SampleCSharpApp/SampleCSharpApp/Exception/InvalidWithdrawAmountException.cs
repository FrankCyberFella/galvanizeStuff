namespace SampleCSharpApp.Exception;

public class InvalidWithdrawAmountException : System.Exception
{
    public InvalidWithdrawAmountException() : base() { }
    public InvalidWithdrawAmountException(string message) : base(message) { }
}