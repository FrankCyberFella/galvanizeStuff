namespace SampleCSharpAppTests.Exception
{

    public class ExcessiveWithdrawAmountException : System.Exception
    {
        public ExcessiveWithdrawAmountException() : base() { }
        public ExcessiveWithdrawAmountException(string message) : base(message) { }
    }
}
