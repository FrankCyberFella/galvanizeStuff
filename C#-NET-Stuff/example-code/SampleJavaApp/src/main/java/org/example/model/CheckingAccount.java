package org.example.model;

//import org.example.exception.ExcessiveWithdrawAmountException;
//import org.example.exception.UnableToAssignAccountNumberException;

import org.example.exception.ExcessiveWithdrawAmountException;
import org.example.exception.InvalidDepositAmountException;
import org.example.exception.InvalidWithdrawAmountException;
import org.example.exception.UnableToAssignAccountNumberException;

import java.text.DecimalFormat;

public class CheckingAccount extends Account{
    public static final double STANDARD_START_BALANCE        = 0;
    public static final double STANDARD_OVERDRAFT_FEE        = 10.00;
    public static final double STANDARD_MAX_OVERDRAFT_AMOUNT = 100.00;

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
        this.maxOverdraftAmount = STANDARD_MAX_OVERDRAFT_AMOUNT;
    }
    public CheckingAccount(String acctOwner, double balance) throws UnableToAssignAccountNumberException {
        super(acctOwner, balance);
        this.overdraftFee       = STANDARD_OVERDRAFT_FEE;
        this.maxOverdraftAmount = STANDARD_MAX_OVERDRAFT_AMOUNT;
    }

    public double getOverdraftFee() {
        return overdraftFee;
    }

    public void setOverdraftFee(double overdraftFee) {
        this.overdraftFee = overdraftFee;
    }

    public double getMaxOverdraftAmount() {
        return maxOverdraftAmount;
    }

    public void setMaxOverdraftAmount(double maxOverdraftAmount) {
        this.maxOverdraftAmount = maxOverdraftAmount;
    }

    @Override
    public double withdraw(double amount) {

        if (amount < 0) {
            throw new InvalidWithdrawAmountException("Deposit amount cannot be less that zero");
        }

        double pendingBalance = getBalance() - amount;
        pendingBalance -= ((pendingBalance < 0) && (pendingBalance <= maxOverdraftAmount)) ? overdraftFee : 0;

        if (pendingBalance >= -maxOverdraftAmount) {
            setBalance(pendingBalance);
        }
        else {
            DecimalFormat amountFormat = new DecimalFormat("$###,###.##");

            String overdrawMessage = String.format("Withdrawal (%s) with overdraft fee (%s) from Balance (%s) would have exceeded allowable overdraft amount (%s)"
                                                  ,amountFormat.format(amount)
                                                  ,amountFormat.format(overdraftFee)
                                                  ,amountFormat.format(getBalance())
                                                  ,amountFormat.format(maxOverdraftAmount));

           throw new ExcessiveWithdrawAmountException(overdrawMessage);
        }

        return getBalance();
    }

    @Override
    public double deposit(double amount) {
        if (amount < 0) {
            throw new InvalidDepositAmountException("Deposit amount cannot be less that zero");
        }
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
