using System;

namespace Lecture
{
    public partial class LectureExample
    {
        /*
        17. Pizza prices are calculated as follows:
                Small cheese pizzas are 8.99, medium cheese are 9.99, and large cheese are 10.99
                Toppings are 1.00 each if 3 or fewer are ordered
                Toppings are 0.75 each if more than 3 are ordered

            Return the price of a pizza based on its size ('s', 'm' or 'l') and number of toppings
            TOPIC: Boolean Expressions & Conditional Logic
         */

        private const decimal Small = 8.99M;
        private const decimal Medium = 9.99M;
        private const decimal Large = 10.99M;
        private const decimal Under_3_Toppings = 1.00M;
        private const decimal Over_3_Toppings = 0.75M;
        public decimal ReturnPizzaCost(char size, int numberOfToppings)
        {
            // You can declare variables in methods. Declare a variable to hold the cost of the pizza.
            // Set its value based on the size. Then add the cost for the toppings and return the total cost
            return 0.0M;
        }
    }
}
