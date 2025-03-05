package unittests;

import exception.ExcessiveWithdrawAmountException;
import model.SafeReflection;
import org.junit.BeforeClass;
import org.junit.Test;

import java.lang.reflect.Constructor;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;

import static org.junit.Assert.*;

public class CheckingAccounTests {

    private static Class chkAccount;

    @BeforeClass
    public static void checkingAccountClassShouldExist() {
        try {
            chkAccount = Class.forName("model.CheckingAccount");
        } catch (Exception e) {
            fail("model.CheckingAccount class does not exist");
        }
    }

    @Test
    public void checkingAccountShouldExtendBankAccount() {
        Class superclass = chkAccount.getSuperclass();
        assertEquals("Checking Account should inherit from Bank Account",superclass.getName(),"model.Account");
    }

    @Test
    public void shouldHaveTwoArgumentConstructor() {
        Constructor constructor = SafeReflection.getConstructor(chkAccount,String.class,double.class);
        assertNotNull("model.CheckingAccount should contain a 2 argument constructor that calls the matching BankAccount constructor.",constructor);
    }

    @Test
    public void shouldHaveThreeArgumentConstructor() {
        Constructor constructor = SafeReflection.getConstructor(chkAccount,String.class,double.class,double.class);
        assertNotNull("model.CheckingAccount should contain a 3 argument constructor that calls the matching BankAccount constructor.",constructor);
    }

    @Test
    public void withdrawNegativeWithFeeBalance_Test() throws IllegalAccessException, InvocationTargetException, InstantiationException, NoSuchMethodException {
        Constructor constructor = SafeReflection.getConstructor(chkAccount,String.class,double.class,double.class, double.class);
        Object checkingAccount = constructor.newInstance("", -1.0, 10.0, 100.0);

        Method withdraw = checkingAccount.getClass().getMethod("withdraw", double.class);
        Method getBalance = checkingAccount.getClass().getMethod("getBalance");
        double newBalance = (double) withdraw.invoke(checkingAccount, 1);
        assertEquals(-12.00, newBalance, .001);
        assertEquals(-12.00, getBalance.invoke(checkingAccount));
    }

    @Test
    public void withdrawPositiveWithFee_Test() throws IllegalAccessException, InvocationTargetException, InstantiationException, NoSuchMethodException {
        Constructor constructor = SafeReflection.getConstructor(chkAccount,String.class,double.class,double.class, double.class);
        Object checkingAccount = constructor.newInstance("", -1.00, 10.00, 100.00);

        Method withdraw = checkingAccount.getClass().getMethod("withdraw", double.class);
        Method getBalance = checkingAccount.getClass().getMethod("getBalance");
        double newBalance = (double) withdraw.invoke(checkingAccount, 2);
        assertEquals(-13.00, newBalance, .001);
        assertEquals(-13.00, (double) getBalance.invoke(checkingAccount), .001);
    }

    @Test
    public void withdrawNegativeBalanceBelowMaxAllowed_Test() throws IllegalAccessException, InvocationTargetException, InstantiationException, NoSuchMethodException {
        Constructor constructor = SafeReflection.getConstructor(chkAccount,String.class,double.class,double.class, double.class);
        Object checkingAccount = constructor.newInstance("", -100.00, 10.00, 100.00);

        Method withdraw = checkingAccount.getClass().getMethod("withdraw", double.class);
        Method getBalance = checkingAccount.getClass().getMethod("getBalance");
        // Using assertThrows with custom exception


        //InvocationTargetException anException = assertThrows(InvocationTargetException.class, () -> withdraw.invoke(checkingAccount,2));
        //assertEquals("Balance would have exceeded allowable overdraft amount", anException.getMessage());

        //assertEquals("Invalid account number format", thrown.getMessage());

        //assertThrows(InvalidAccountNumberException.class, () -> account.setAccountNumber(null));



        //assertThrows(ExcessiveWithdrawAmountException.class, () -> withdraw.invoke(checkingAccount,2));
        /*
        assertThrows(Exception.class, () -> {
            try {
                withdraw.invoke(checkingAccount,2.0);
            } catch (InvocationTargetException e) {
                throw (Exception) e.getCause();
            } catch (ExcessiveWithdrawAmountException e) {
                throw new Exception(e);
            }
        });
        */

        /*
        double newBalance = (double) withdraw.invoke(checkingAccount, 2);
        assertEquals(-100.00, newBalance, .001);
        assertEquals(-100.00, (double) getBalance.invoke(checkingAccount), .001);
         */
    }

    @Test
    public void withdrawPositiveBalance_Test() throws IllegalAccessException, InvocationTargetException, InstantiationException, NoSuchMethodException {
        Constructor constructor = SafeReflection.getConstructor(chkAccount,String.class,double.class,double.class, double.class);
        Object checkingAccount = constructor.newInstance("",10.00, 10.00, 100.00);

        Method withdraw = checkingAccount.getClass().getMethod("withdraw", double.class);
        Method getBalance = checkingAccount.getClass().getMethod("getBalance");
        double newBalance = (double) withdraw.invoke(checkingAccount, 5);
        assertEquals(5.00, newBalance, .001);
        assertEquals(5.00, (double) getBalance.invoke(checkingAccount), .001);
    }
}
