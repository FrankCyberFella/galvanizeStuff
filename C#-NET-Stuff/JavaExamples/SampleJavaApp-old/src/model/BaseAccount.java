package model;

public interface BaseAccount {

    public double getBalance();
    public double withdraw(double amount);
    public double deposit(double amount);

}
