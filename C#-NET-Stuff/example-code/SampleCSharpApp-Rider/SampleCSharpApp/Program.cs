using System;
using System.IO;
using System.Collections.Generic;

namespace franksmenu
{
    public class Menu
    {
        private string menuName;

        public void setMenuName(string name)
        {
            this.menuName = name;
        }
        
        public string getMenuName()
        {
            return this.menuName;
        }
    }

    public class MenuService
    {
        private TextReader input;
        private TextWriter output;
        private Dictionary<string, List<Tuple<string, Action>>> menus = new Dictionary<string, List<Tuple<string, Action>>>();

        public MenuService(TextReader input, TextWriter output)
        {
            this.input = input;
            this.output = output;
        }

        public void AddMenuItem(string menuName, string description, Action action)
        {
            if (!menus.ContainsKey(menuName))
            {
                menus[menuName] = new List<Tuple<string, Action>>();
            }
            menus[menuName].Add(new Tuple<string, Action>(description, action));
        }

        public void displayMenu(string menuName)
        {
            if (!menus.ContainsKey(menuName))
            {
                output.WriteLine("Menu not found: " + menuName);
                return;
            }
            output.WriteLine("== " + menuName + " ==");
            List<Tuple<string, Action>> items = menus[menuName];
            for (int i = 0; i < items.Count; i++)
            {
                output.WriteLine($"{i + 1}. {items[i].Item1}");
            }
            output.WriteLine("Enter the number of your choice:");
            string choiceStr = input.ReadLine();
            int choice;
            if (int.TryParse(choiceStr, out choice))
            {
                if (choice > 0 && choice <= items.Count)
                {
                    // Execute the selected action.
                    items[choice - 1].Item2();
                }
                else
                {
                    output.WriteLine("Invalid choice. Exiting menu.");
                }
            }
            else
            {
                output.WriteLine("Invalid input. Exiting menu.");
            }
        }
    }
}

namespace org.example.exception
{
    public class UnableToAssignAccountNumberException : Exception
    {
        public UnableToAssignAccountNumberException() : base() { }
        public UnableToAssignAccountNumberException(string message) : base(message) { }
        public UnableToAssignAccountNumberException(string message, Exception inner) : base(message, inner) { }
    }
}

namespace org.example.model
{
    public abstract class Account
    {
        private static int nextAccountNumber = 1000;
        protected int acctNumber;
        protected string acctOwner;
        protected double balance;

        public Account(string acctOwner, double balance)
        {
            this.acctOwner = acctOwner;
            this.balance = balance;
            try
            {
                this.acctNumber = nextAccountNumber++;
            }
            catch (Exception ex)
            {
                throw new org.example.exception.UnableToAssignAccountNumberException("Unable to assign account number.", ex);
            }
        }

        public int getAcctNumber()
        {
            return acctNumber;
        }

        public string getAcctOwner()
        {
            return acctOwner;
        }

        public double getBalance()
        {
            return balance;
        }
    }

    public class CheckingAccount : Account
    {
        private double overdraftFee;
        private int maxOverdraftAmount;

        public CheckingAccount(string acctOwner, double balance, double overdraftFee, int maxOverdraftAmount) : base(acctOwner, balance)
        {
            this.overdraftFee = overdraftFee;
            this.maxOverdraftAmount = maxOverdraftAmount;
        }

        public CheckingAccount(string acctOwner, double balance) : base(acctOwner, balance)
        {
            // Default values for CheckingAccount when only owner and balance are provided.
            this.overdraftFee = 0.0;
            this.maxOverdraftAmount = 0;
        }

        public int getMaxOverdraftAmount()
        {
            return maxOverdraftAmount;
        }

        public double getOverdraftFee()
        {
            return overdraftFee;
        }
    }

    public class SavingsAccount : Account
    {
        private double annualInterestRate;

        public SavingsAccount(string acctOwner, double balance) : base(acctOwner, balance)
        {
            // Default annual interest rate set to 2% for SavingsAccount.
            this.annualInterestRate = 0.02;
        }

        public double getAnnualInterestRate()
        {
            return annualInterestRate;
        }
    }
}

namespace org.example
{
    using franksmenu;
    using org.example.exception;
    using org.example.model;

    public class SampleJavaApp
    {
        private static SortedDictionary<int, Account> theAccounts = new SortedDictionary<int, Account>();

        public static void Main(string[] args)
        {
            try
            {
                Console.WriteLine("Hello and welcome to our Sample Java App!");

                SetupTestAccounts();
                
                DisplayAllAccounts();
                
                return;
            }
            catch (UnableToAssignAccountNumberException ex)
            {
                Console.WriteLine("Error: " + ex.Message);
            }
        }

        static void DisplayAllAccounts()
        {
            foreach (KeyValuePair<int, Account> anEntry in theAccounts)
            {
                DisplayAccountDetails(anEntry.Value);
            }
        }

        static void DisplayAccountDetails(Account anAccount)
        {
            Console.WriteLine(new string('$', 50));
            string className = anAccount.GetType().Name;
            Console.WriteLine("  Account Type: " + className);
            Console.WriteLine("Account Number: " + anAccount.getAcctNumber());
            Console.WriteLine(" Account Owner: " + anAccount.getAcctOwner());
            Console.WriteLine("       Balance: " + anAccount.getBalance());
            switch (className)
            {
                case "CheckingAccount":
                    {
                        CheckingAccount aCheckAcct = (CheckingAccount)anAccount;
                        Console.WriteLine("Max Overdraft Amt: " + aCheckAcct.getMaxOverdraftAmount());
                        Console.WriteLine("    Overdraft Fee: " + aCheckAcct.getOverdraftFee());
                        break;
                    }
                case "SavingsAccount":
                    {
                        SavingsAccount aSaveAcct = (SavingsAccount)anAccount;
                        Console.WriteLine("           APR: " + aSaveAcct.getAnnualInterestRate() * 100 + "%");
                        break;
                    }
            }
        }

        static void displayWithdrawExceptionMessage(Exception anException, Account anAccount)
        {
            Console.WriteLine(new string('!', 50));
            Console.WriteLine("!!!!! Uh-OH Uh-Oh Uh-Oh !!!!!");
            Console.WriteLine("Withdrawal denied - Acct #: " + anAccount.getAcctNumber() + " Owner: " + anAccount.getAcctOwner());
            Console.WriteLine(anException.Message);
            Console.WriteLine(new string('!', 50));
        }

        static public void SetupTestAccounts()
        {
            Account a1 = new CheckingAccount("Frank", 0.00, 15.00, 100);
            theAccounts.Add(a1.getAcctNumber(), a1);
            Account a2 = new SavingsAccount("Worf", 100);
            theAccounts.Add(a2.getAcctNumber(), a2);
            Account a3 = new CheckingAccount("Data", 200);
            theAccounts.Add(a3.getAcctNumber(), a3);
        }
    }
}
