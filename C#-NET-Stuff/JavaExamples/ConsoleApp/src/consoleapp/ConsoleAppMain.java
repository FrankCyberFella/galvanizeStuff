package consoleapp;

import consoleapp.service.MenuService;

public class ConsoleAppMain {
    public static void main(String[] args) {

        MenuService menu = new MenuService(System.in, System.out);  // Define Menu object with input and output sources (keyboard, screen)


        menu.AddMenuItem("Frank", "Item 1");
        menu.AddMenuItem("Frank", "Item 2");
        menu.AddMenuItem("Frank", "Item 3");

        menu.displayMenu("Frank");

        System.out.println("\n*************************************");
        System.out.println("   Thanks for your participation");
        System.out.println("       Have a Java-rific day!");
        System.out.println("*************************************");
    }
}