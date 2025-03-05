package franksmenu;

import callbackfunction.CallbackFunction;

public class MenuItem {
    String MenuItem;
    CallbackFunction menuItemProcess;

    public MenuItem(String menuItem) {
        MenuItem = menuItem;
        menuItemProcess = null;
    }
    public MenuItem(String menuItem, CallbackFunction processingFunction) {
        MenuItem = menuItem;
        menuItemProcess = processingFunction;
    }


    public CallbackFunction getMenuItemProcess() {
        return menuItemProcess;
    }

    public void setMenuItemProcess(CallbackFunction menuItemProcess) {
        this.menuItemProcess = menuItemProcess;
    }

    public String getMenuItem() {
        return MenuItem;
    }

    public void setMenuItem(String menuItem) {
        MenuItem = menuItem;
    }
}
