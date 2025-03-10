package org.example.exception;

public class InvalidDepositAmountException extends RuntimeException{

    public InvalidDepositAmountException() {super(); }
    public InvalidDepositAmountException(String message) {super(message);}

    }

