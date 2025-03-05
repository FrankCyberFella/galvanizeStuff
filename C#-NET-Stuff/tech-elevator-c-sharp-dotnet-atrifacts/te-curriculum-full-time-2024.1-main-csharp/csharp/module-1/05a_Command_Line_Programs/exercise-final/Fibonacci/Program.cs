using System;

namespace Fibonacci
{
    class Program
    {
        static void Main(string[] args)
        {
            Console.Write("Please enter a number: ");
            int limit = int.Parse(Console.ReadLine());
            int previous = 1;

            Console.Write("0, 1");

            for (int next = 1; next <= limit;)
            {
                Console.Write(", " + next);

                int temp = previous + next;
                previous = next;
                next = temp;
            }

            Console.WriteLine();
        }
    }
}
