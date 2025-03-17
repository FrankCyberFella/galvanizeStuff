namespace SampleCSharpApp.Exception;

public class UnableToAssignAccountNumberException : System.Exception
{
    public UnableToAssignAccountNumberException() : base() { }
    public UnableToAssignAccountNumberException(string message) : base(message) { }
}
