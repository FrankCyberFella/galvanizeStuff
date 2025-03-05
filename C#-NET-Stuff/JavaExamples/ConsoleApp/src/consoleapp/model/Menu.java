package consoleapp.model;

import java.util.List;

public class Menu {
    private String       menuName;
    private String       menuPrompt;
    private List<String> menuItems;

    public String getMenuName() {
        return menuName;
    }
    public void setMenuName(String menuName) {
        this.menuName = menuName;
   0 }

    public String getMenuPrompt() {
        return menuPrompt;
    }
    public void setMenuPrompt(String menuPrompt) {
        this.menuPrompt = menuPrompt;
    }

    public List<String> getMenuItems() {
        return menuItems;
    }
    public void setMenuItems(List<String> menuItems) {
        this.menuItems = menuItems;
    }
}
    

