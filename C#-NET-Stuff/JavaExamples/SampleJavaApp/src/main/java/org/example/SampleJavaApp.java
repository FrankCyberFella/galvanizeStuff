package org.example;

import franksmenu.Menu;
import franksmenu.MenuService;
import org.example.exception.UnableToAssignAccountNumberException;
import org.example.model.Account;
import org.example.model.CheckingAccount;
import org.example.model.SavingsAccount;

import java.util.Map;
import java.util.TreeMap;

public class SampleJavaApp {

    private static Map<Integer, Account> theAccounts = new TreeMap<Integer,Account>();

    public static void main(String[] args) throws UnableToAssignAccountNumberException {

        System.out.println("Hello and welcome to our Sample Java App!");

        MenuService appMenus = new MenuService(System.in, System.out);
        final String SERVICES_MENU = "ServicesMenu";
        Menu AccountServicesMenu = new Menu();
        AccountServicesMenu.setMenuName("Galactic Bank Services Menu");

        SetupTestAccounts();

          appMenus.AddMenuItem(SERVICES_MENU, "Display All Accounts", displayAllAccounts());
//        appMenus.AddMenuItem(SERVICES_MENU, "Add An Account");
//        appMenus.AddMenuItem(SERVICES_MENU, "Display a specific account by Account Number");
//        appMenus.AddMenuItem(SERVICES_MENU, "Find all accounts for a specifc owner");
//        appMenus.AddMenuItem(SERVICES_MENU, "Deposit / Withdraw from an account");
//        appMenus.AddMenuItem(SERVICES_MENU, "Exit ");



        appMenus.displayMenu("ServicesMenu");


        return;

    }

    static void displayAllAccounts() {
        for (Map.Entry<Integer, Account> anEntry : theAccounts.entrySet()){
            displayAccountDetails(anEntry.getValue());
        }
    }

    static void displayAccountDetails(Account anAccount) {
        System.out.println("$".repeat(50));
        String className = anAccount.getClass().getName().substring(anAccount.getClass().getName().lastIndexOf(".")+1);
        System.out.println("  Account Type: " + className);
        System.out.println("Account Number: " + anAccount.getAcctNumber());
        System.out.println(" Account Owner: " + anAccount.getAcctOwner());
        System.out.println("       Balance: " + anAccount.getBalance());
        switch (className) {
            case "CheckingAccount": {
                CheckingAccount aCheckAcct = (CheckingAccount) anAccount;
                System.out.println("Max Overdraft Amt: " + aCheckAcct.getMaxOverdraftAmount());
                System.out.println("    Overdraft Fee: " + aCheckAcct.getOverdraftFee());
                break;
            }
            case "SavingsAccount": {
                SavingsAccount aSaveAcct = (SavingsAccount) anAccount;
                System.out.println("           APR: " + aSaveAcct.getAnnualInterestRate()*100 + "%");
                break;
            }

        }
    }

    static void displayWithdrawExceptionMessage(Exception anException, Account anAccount) {
        System.out.println("!".repeat(50));
        System.out.println("!!!!! Uh-OH Uh-Oh Uh-Oh !!!!!");
        System.out.println("Withdrawal denied - Acct #: " + anAccount.getAcctNumber() + " Owner: " + anAccount.getAcctOwner());
        System.out.println(anException.getMessage());
        System.out.println("!".repeat(50));
    }

    static public void SetupTestAccounts() throws UnableToAssignAccountNumberException {
        Account a1 = new CheckingAccount("Frank", 0.00, 15.00, 100);
        theAccounts.put(a1.getAcctNumber(), a1);
        Account a2 = new SavingsAccount("Worf", 100);
        theAccounts.put(a2.getAcctNumber(), a2);
        Account a3 = new CheckingAccount("Data", 200);
        theAccounts.put(a3.getAcctNumber(), a3);
    }
}