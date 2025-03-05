package consoleapp.service;

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

    public void AddMenuItem(String menuId, String menuItem) {
        if(Menus.containsKey(menuId)) {
            List<String> existingMenu = Menus.get(menuId);
            existingMenu.add(menuItem);
            Menus.put(menuId, existingMenu);
        }
        List<String> newMenu = new ArrayList<String>();
        newMenu.add(menuItem);
        Menus.put(menuId, newMenu);
    }
    public void displayMenu(String menuId) {
        List<String> theMenu = Menus.get(menuId);
        System.out.println("Items ins menuId: " + menuId);
        for(int i=0; i < theMenu.size(); i++) {
            System.out.println(i + ". " + theMenu.get(i));
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

