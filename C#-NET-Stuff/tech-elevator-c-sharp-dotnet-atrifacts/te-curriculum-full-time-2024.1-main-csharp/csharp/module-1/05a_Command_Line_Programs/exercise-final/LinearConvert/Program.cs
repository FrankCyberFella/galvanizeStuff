using System;

namespace LinearConvert
{
    class Program
    {
        static void Main(string[] args)
        {
            const double FeetToMeters = .3048;
            const double MetersToFeet = 3.2808399;

            Console.Write("Please enter the length: ");
            string lengthInput = Console.ReadLine();
            int length = int.Parse(lengthInput);

            Console.Write("Is the measurement in (m)eters, or (f)eet?: ");
            string unitInput = Console.ReadLine();

            if ("f" == unitInput)
            {
                int meterLength = (int)(length * FeetToMeters);
                Console.WriteLine(length + "f is " + meterLength + "m.");
            }
            else if ("m" == unitInput)
            {
                int feetLength = (int)(length * MetersToFeet);
                Console.WriteLine(length + "m is " + feetLength + "f.");
            }
            else
            {
                Console.WriteLine(unitInput + " is an invalid choice.");
            }
        }
    }
}
