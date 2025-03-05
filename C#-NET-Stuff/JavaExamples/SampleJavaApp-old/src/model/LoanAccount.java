package model;

import exception.UnableToAssignAccountNumberException;

public class LoanAccount extends Account {

    private double annualInterestRate;
    private int durationInMonths;
    private double lateFee;
    private int dayOfMonthDue;

    public LoanAccount(String acctOwner, double balance) throws UnableToAssignAccountNumberException {
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
        return super.toString() + ";\nLoanAccount{" +
                "annualInterestRate=" + annualInterestRate +
                ", durationInMonths=" + durationInMonths +
                ", lateFee=" + lateFee +
                ", dayOfMonthDue=" + dayOfMonthDue +
                '}';
    }
}
