namespace SampleCSharpApp.Exception;

public class InvalidDepositAmountException : System.Exception
{
    public InvalidDepositAmountException() : base() { }
    public InvalidDepositAmountException(string message) : base(message) { }
}