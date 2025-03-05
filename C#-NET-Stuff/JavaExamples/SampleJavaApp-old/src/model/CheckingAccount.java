package model;

import exception.ExcessiveWithdrawAmountException;
import exception.UnableToAssignAccountNumberException;

public class CheckingAccount extends Account{
    private final double STANDARD_OVERDRAFT_FEE    = 10.00;
    private final double STANDARD_OVERDRAFT_AMOUNT = 100.00;

    private double overdraftFee;
    private double maxOverdraftAmount;

    public CheckingAccount(String acctOwner, double balance, double overdraftFee, double maxOverdraftAmount) throws UnableToAssignAccountNumberException {
        super(acctOwner, balance);
        this.overdraftFee       = overdraftFee;
        this.maxOverdraftAmount = maxOverdraftAmount;
    }
    public CheckingAccount(String acctOwner, double balance, double overdraftFee) throws UnableToAssignAccountNumberException {
        super(acctOwner, balance);
        this.overdraftFee       = overdraftFee;
        this.maxOverdraftAmount = STANDARD_OVERDRAFT_AMOUNT;
    }
    public CheckingAccount(String acctOwner, double balance) throws UnableToAssignAccountNumberException {
        super(acctOwner, balance);
        this.overdraftFee       = STANDARD_OVERDRAFT_FEE;
        this.maxOverdraftAmount = STANDARD_OVERDRAFT_AMOUNT;
    }
    @Override
    public double withdraw(double amount) {
        double pendingBalance = getBalance() - amount;
        pendingBalance -= ((pendingBalance < 0) && (pendingBalance <= maxOverdraftAmount)) ? overdraftFee : 0;

        if (pendingBalance >= -maxOverdraftAmount) {
            setBalance(pendingBalance);
        }
        else {
           throw new ExcessiveWithdrawAmountException("Balance would have exceeded allowable overdraft amount ("
                                                     + maxOverdraftAmount +")");
        }

        return getBalance();
    }

    @Override
    public double deposit(double amount) {
        setBalance(getBalance()+amount);
        return getBalance();
    }

    @Override
    public String toString() {
        return super.toString() + ";\nCheckingAccount{" +
                "overdraftFee=" + overdraftFee +
                ", maxOverdraftAmount=" + maxOverdraftAmount +
                '}';
    }
}
