package model;

import com.sun.jdi.InternalException;
import exception.UnableToAssignAccountNumberException;

import java.util.*;

public abstract class Account implements BaseAccount {
    private static Set<Integer> existingAccts;
    private static int          maxAcctNumAttemps = 10000;

    private int    acctNumber;
    private String acctOwner;
    private double balance;



    static {
        existingAccts = new TreeSet<Integer>();
    }


    public Account(String acctOwner,  double balance) throws UnableToAssignAccountNumberException {
        this.acctNumber = getUniqueAcctNumber();
        this.acctOwner = acctOwner;
        this.balance = balance;
    }

    private int getUniqueAcctNumber() throws UnableToAssignAccountNumberException {
        int newAcctNum = 0;
        Random randomizer = new Random();
        boolean foundUniqueAcctNum = false;
        int loopCount = 0;
        while ((!foundUniqueAcctNum) && (loopCount < maxAcctNumAttemps)) {
            newAcctNum = randomizer.nextInt(1000000, 999999999);
            foundUniqueAcctNum = existingAccts.add(newAcctNum);
            loopCount++;
        }
        if (foundUniqueAcctNum)
            return newAcctNum;
        else
            throw new UnableToAssignAccountNumberException("Unable to find unique account number - Account not created");
    }

    @Override
    public String toString() {
        return "Account{" +
                "acctNumber=" + acctNumber +
                ", acctOwner='" + acctOwner + '\'' +
                ", balance=" + balance +
                '}';
    }

    @Override
    public double getBalance() {
        return balance;
    };

    public int getAcctNumber() {
        return acctNumber;
    }

    public void setAcctNumber(int acctNumber) {
        this.acctNumber = acctNumber;
    }

    public String getAcctOwner() {
        return acctOwner;
    }

    public void setAcctOwner(String acctOwner) {
        this.acctOwner = acctOwner;
    }

    public void setBalance(double balance) {
        this.balance = balance;
    }

    @Override
    public abstract double withdraw(double amount);

    @Override
    public abstract double deposit(double amount);
}

