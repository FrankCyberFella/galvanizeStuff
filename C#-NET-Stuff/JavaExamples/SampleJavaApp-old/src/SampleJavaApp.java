import exception.UnableToAssignAccountNumberException;
import model.Account;
import model.CheckingAccount;

public class SampleJavaApp {
    public static void main(String[] args) throws UnableToAssignAccountNumberException {

        System.out.println("Hello and welcome!");

        Account a1 = new CheckingAccount("Frank", 0.00, 15.00, 250.00);

        System.out.println("\nAcct #; " + a1.getAcctNumber() + "  currrent balance: " + a1.getBalance());
        a1.deposit(100);
        System.out.println("\nAcct #; " + a1.getAcctNumber() + "  currrent balance: " + a1.getBalance());

        System.out.println("\nAcct #; " + a1.getAcctNumber() + "  currrent balance: " + a1.getBalance());
        a1.withdraw((2300));
        System.out.println("\nAcct #; " + a1.getAcctNumber() + "  currrent balance: " + a1.getBalance());

 //       Account a2 = new Account("acct2", 110);
 //       Account a3 = new Account("acct3", 210);
 //       Account a4 = new Account("acct4", 310);
 //       Account a5 = new Account("acct5", 410);

        System.out.println(a1);
  //      System.out.println(a2);
   //     System.out.println(a3);
//       System.out.println(a4);
  //      System.out.println(a5);


    }
}