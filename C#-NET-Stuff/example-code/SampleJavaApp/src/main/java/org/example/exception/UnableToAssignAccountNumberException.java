package org.example.exception;

public class UnableToAssignAccountNumberException extends Exception{
    public UnableToAssignAccountNumberException() {super(); }
    public UnableToAssignAccountNumberException(String message) {
        super(message);
    }
}
