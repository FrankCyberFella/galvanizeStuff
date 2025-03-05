package model;

import exception.UnableToAssignAccountNumberException;

public class SavingsAccount extends Account{

    private double annualInterestrate = .05;

    public SavingsAccount(String acctOwner, double balance, double annualInterestrate) throws UnableToAssignAccountNumberException {
        super(acctOwner, balance);
    }

    @Override
    public double withdraw(double amount) {
        return 0;
    }

    @Override
    public double deposit(double amount) {
        return 0;
    }

    @Override
    public String toString() {
        return super.toString() + "; \nSavingsAccount{" +
                "annualInterestrate=" + annualInterestrate +
                '}';
    }
}
