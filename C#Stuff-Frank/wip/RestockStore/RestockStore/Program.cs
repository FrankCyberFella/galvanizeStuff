using System.Numerics;

namespace RestockStore;

class Program
{
    static void Main(string[] args)
    {
        int sodas = 100;
        int sodaRestock = 40;
        ;
        int chips = 40;
        int chipsRestock = 20;
        
        int candy = 60;
        int candyRestock = 40;

        string userResponse = "";

        Console.WriteLine("Welcome to Store Restocking App");

        Console.Write("How many sodas have been sold today? " + sodas + " in stock");
        userResponse = Console.ReadLine();

        if (int.Parse(userResponse) <= sodas) {
            sodas = sodas - int.Parse(userResponse);
            Console.WriteLine("There are " + sodas +" left");
        }
        else {
              Console.WriteLine("Number entered is too high");
        }

    }
}



