package franksmenu;

import callbackfunction.CallbackFunction;

import java.io.InputStream;
import java.io.OutputStream;
import java.io.PrintWriter;
import java.util.*;

public class MenuService {

    private static Map<String, Menu> Menus;

    private PrintWriter out;
    private Scanner in;

    public MenuService(InputStream input, OutputStream output) {
        this.out = new PrintWriter(output);
        this.in = new Scanner(input);
        this.Menus = new TreeMap<String, Menu>();
    }

    public void addMenu(String menuId, Menu aMenu) {
        Menus.put(menuId, aMenu);
    }

    public void AddMenuItem(String menuId, String menuOption, CallbackFunction functionToProcess) {
        this.AddMenuItem(menuId, new MenuItem(menuOption, functionToProcess));
    }

    public void AddMenuItem(String menuId, MenuItem menuItem) {
        if(Menus.containsKey(menuId)) {
            Menu existingMenu = Menus.get(menuId);
            existingMenu.addMenuItem(menuItem);
            Menus.put(menuId, existingMenu);
        }
        else {
            Menu newMenu = new Menu();
            newMenu.addMenuItem(menuItem);
            Menus.put(menuId, newMenu);
        }
    }
    public void displayMenu(String menuId) {
        Menu theMenu = Menus.get(menuId);
        System.out.println("Items in menuId: " + menuId);
        for(int i=1; i < theMenu.getMenuItems().size()+1; i++) {
            System.out.println(i + ". " + theMenu.getMenuItems().get(i-1));
        }
    }
    public void displayMenu(String menuId, boolean withMenuId) {
        Menu theMenu = Menus.get(menuId);
        if (withMenuId) {
            System.out.println("Items in menuId: " + menuId);
        }
        for(int i=1; i < theMenu.getMenuItems().size()+1; i++) {
            System.out.println(i + ". " + theMenu.getMenuItems().get(i-1));
        }
    }

    public Object getChoiceFromOptions(Object[] options) {
        Object choice = null;
        while(choice == null) {
            displayMenuOptions(options);
            choice = getChoiceFromUserInput(options);
        }
        return choice;
    }

    private Object getChoiceFromUserInput(Object[] options) {
        Object choice = null;
        String userInput = in.nextLine();
        try {
            int selectedOption = Integer.valueOf(userInput);
            if(selectedOption <= options.length) {
                choice = options[selectedOption - 1];
            }
        } catch(NumberFormatException e) {
            // eat the exception, an error message will be displayed below since choice will be null
        }
        if(choice == null) {
            out.println("\n*** "+userInput+" is not a valid option ***\n");
        }
        return choice;
    }

    private void displayMenuOptions(Object[] options) {
        out.println();
        for(int i = 0; i < options.length; i++) {
            int optionNum = i+1;
            out.println(optionNum+") "+options[i]);
        }
        out.print("\nPlease choose an option >>> ");
        out.flush();
    }
}

