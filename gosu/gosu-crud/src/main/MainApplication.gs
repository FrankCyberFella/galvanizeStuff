package main

uses model.Item
uses resource.Option

class MainApplication {

  var _items : List<Item>as Items

  static function main(arg : String[]) {

    System.out.println("[MainApplication] Started..");
    var input = new Scanner(System.in)

    // Init
    var app = new MainApplication()
    app.Items = app.getInitialItems()

    // 1. Display option
    var options = Option.renderToString()

    // 2. Key in option
    var selectedOption = 0;
    var isExit = false;

    while (!isExit) {

      System.out.println();
      System.out.println(options);

      while (!input.hasNextInt()) {
        System.out.println("Invalid input " + input.next());
      }
      selectedOption = input.nextInt();


      switch (selectedOption) {
        case 1:
          app.create(input);
          break;
        case 2:
          app.update(input);
          break;
        case 3:
          app.read();
          break;
        case 4:
          app.delete(input);
          break;
        case -1:
          isExit = true;
          break;
        default:
          System.out.println("Invalid option " + selectedOption);
          break;
      }


    }

    System.out.println("[MainApplication] End..");

  }


  function getInitialItems() : List<Item> {

    var items : List<Item> = new ArrayList<Item>()
    items.add(new Item('ID1', 'Item 1', 'This is item 1', new Date(), 'SYSTEM'))
    items.add(new Item('ID2', 'Item 2', 'This is item 2', new Date(), 'SYSTEM'))
    items.add(new Item('ID3', 'Item 3', 'This is item 3', new Date(), 'SYSTEM'))
    return items
  }

  private function create(scanner : Scanner) : void {

    var item = new Item()

    while (true) {
      print("ID : ") // how to inline print
      var id = scanner.next()
      item.Id = id

      if (!this.Items.contains(item)) {
        break;
      }
      System.out.println("Item with ID " + id + " already exist. Try again.");
    }

    scanner.nextLine()
    print("Name : ")
    item.Name = scanner.nextLine()

    print("Description : ")
    item.Description = scanner.nextLine()

    item.CreatedBy = "CurrentUser"
    item.CreatedDate = new Date()

    this._items.add(item)
    return

  }

  private function read() : void {
    for (item in this.Items) {
      print(item)
    }
  }

  private function update(scanner : Scanner) : void {

    print("Enter Item ID to update : ")
    var id = scanner.next()

    var item = new Item()
    item.Id = id

    item = this.Items.get(this.Items.indexOf(item))


    if (item == null) {
      print("Id ${id} doesn't exist")
      return
    }

    print("Name : ")
    item.Name = scanner.next()

    print("Description : ")
    item.Description = scanner.next()

    item.UpdatedBy = "CurrentUser"
    item.UpdatedDate = new Date()
    return

  }


  private function delete(input : Scanner) : void {
    System.out.print("Enter Item ID to delete : ");
    var id = input.next();

    var selectedItem = new Item();
    selectedItem.Id = id

    if (!this.Items.contains(selectedItem)) {
      System.out.println("Item with ID " + id + " does not exist.");
    } else {
      this.Items.remove(selectedItem);
      System.out.println("Successfully deleted " + id);
    }

  }
}