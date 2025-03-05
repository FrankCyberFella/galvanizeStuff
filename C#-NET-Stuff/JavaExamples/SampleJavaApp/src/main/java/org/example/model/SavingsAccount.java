package org.example.model;

import org.example.exception.InvalidDepositAmountException;
import org.example.exception.InvalidWithdrawAmountException;
import org.example.exception.UnableToAssignAccountNumberException;

public class SavingsAccount extends Account{

    public static final double SAVINGS_DEFAULT_BALANCE = 0;
    public static final double SAVINGS_DEFAULT_APR     = .01;

    private double annualInterestRate;

    public SavingsAccount(String acctOwner, double balance, double annualInterestRate) throws UnableToAssignAccountNumberException {
        super(acctOwner, balance);
        this.annualInterestRate = annualInterestRate;
    }
    public SavingsAccount(String acctOwner, double balance) throws UnableToAssignAccountNumberException {
        super(acctOwner, balance);
        this.annualInterestRate = SAVINGS_DEFAULT_APR;
    }
    public SavingsAccount(String acctOwner) throws UnableToAssignAccountNumberException {
        super(acctOwner, SAVINGS_DEFAULT_BALANCE);
        this.annualInterestRate = SAVINGS_DEFAULT_APR;
    }

    public double getAnnualInterestRate() {
        return annualInterestRate;
    }

    public void setAnnualInterestRate(double annualInterestRate) {
        this.annualInterestRate = annualInterestRate;
    }

    @Override
    public double withdraw(double amount) {
        if (amount > balance || amount < 0) {
            throw new InvalidWithdrawAmountException("Withdrawal amount ("+ amount + ") exceeds balance (" + balance + ") or is less than 0");
        }
        return balance -= amount;
    }

    @Override
    public double deposit(double amount) {
        if (amount < 0) {
            throw new InvalidDepositAmountException("Withdrawal amount (" + amount + ") cannot be less than 0");
        }
        return balance += amount;
    }

    public double applyMonthlyInterest() {
        return balance += balance * (annualInterestRate / 12);
    }

    @Override
    public String toString() {
        return super.toString() + "; \nSavingsAccount{" +
                "annualInterestRate=" + annualInterestRate +
                '}';
    }
}
