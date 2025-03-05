package franksmenu;

import java.util.ArrayList;
import java.util.List;

public class Menu {
    private String         menuName;
    private String         menuPrompt;
    private List<MenuItem> menuItems = null;

    public String getMenuName() {
        return menuName;
    }
    public void   setMenuName(String menuName) {
        this.menuName = menuName;
    }

    public String getMenuPrompt() {
        return menuPrompt;
    }
    public void   setMenuPrompt(String menuPrompt) {
        this.menuPrompt = menuPrompt;
    }

    public List<String> getMenuItems() { return new ArrayList(menuItems); }

    public void addMenuItem(MenuItem aNewItem) {
        if (this.menuItems == null) {
            this.menuItems = new ArrayList<>();
        }
        this.menuItems = menuItems;menuItems.add(aNewItem);
    }
}
    

