package org.example.exception;

public class ExcessiveWithdrawAmountException extends RuntimeException{
    public ExcessiveWithdrawAmountException() {super(); }
    public ExcessiveWithdrawAmountException(String message) {super(message);}
}
