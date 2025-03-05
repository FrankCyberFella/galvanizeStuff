using System;
using System.Collections.Generic;
using System.Text;

namespace Exercises
{
    public class Exercise02_BoardingGate
    {
        /*
        Local Jetways is a regional airline operating at local airports.
        They use a basic passenger manifest to represent seat occupancy on their plane.
        Each passenger seat is represented as an element in a boolean array.
        A value of IsAvailable (true) indicates that seat is currently available.
        A value of IsOccupied (false) indicates the seat is not available.
        */
        private const bool IsAvailable = true;
        private const bool IsOccupied = false;


        /*
        A nearby airport has an incoming flight from Local Jetways. As the passengers disembark, the gate
        attendant's first responsibility is to generate a new seating chart with each seat initially available.

        Implement the logic to generate a seating chart using the required number of seats. Make sure to indicate
        that each seat is initially available.

        Note: The number of seats is guaranteed not to be negative.

        Examples:
        GenerateSeatingChart(7) → [IsAvailable, IsAvailable, IsAvailable, IsAvailable, IsAvailable, IsAvailable, IsAvailable]
        GenerateSeatingChart(5) → [IsAvailable, IsAvailable, IsAvailable, IsAvailable, IsAvailable]
        GenerateSeatingChart(2) → [IsAvailable, IsAvailable]
        */
        public bool[] GenerateSeatingChart(int numberOfSeats)
        {
            bool[] seatingChart = new bool[numberOfSeats];

            for (int i = 0; i < seatingChart.Length; i++)
            {
                seatingChart[i] = IsAvailable;
            }

            return seatingChart;

        }

        /*
        Once passengers begin boarding the plane, gate attendants need a way to determine how many available
        seats there are on the plane.

        Using the array provided, implement the logic to count the number of available seats in the seating chart. 

        Examples:
        GetAvailableSeatCount([IsAvailable, IsOccupied, IsOccupied, IsOccupied]) → 1
        which is the same as:
        GetAvailableSeatCount([true, false, false, false]) → 1
        GetAvailableSeatCount([IsOccupied, IsOccupied, IsOccupied, IsOccupied, IsOccupied, IsOccupied]) → 0
        GetAvailableSeatCount([IsAvailable, IsAvailable, IsAvailable, IsOccupied]) → 3
        GetAvailableSeatCount([]) → 0
        */
        public int GetAvailableSeatCount(bool[] seatingChart)
        {
            int count = 0;

            for (int i = 0; i < seatingChart.Length; i++)
            {
                if (seatingChart[i] == IsAvailable)
                {
                    count++;
                }
            }

            return count;

        }

        /*
        The crew determined that it would be nice to know how many rows on the plane are at full occupancy.
        Each row has three seats and a row is at full occupancy if all three seats have someone sitting in them.

        Using the boolean array, implement the logic to count the number of full rows on the plane.
        Note: A new row starts at every third element. For example, row one begins with index 0, row two begins with index 3, and so on.

        Examples:
        GetNumberOfFullRows([IsOccupied, IsOccupied, IsOccupied, IsOccupied, IsOccupied, IsOccupied]) → 2
        GetNumberOfFullRows([IsOccupied, IsOccupied, IsOccupied, IsAvailable, IsAvailable, IsAvailable]) → 1
        GetNumberOfFullRows([IsAvailable, IsAvailable, IsAvailable, IsAvailable, IsAvailable, IsAvailable]) → 0    
        */

        public int GetNumberOfFullRows(bool[] seatingChart)
        {
            int fullRows = 0;
            int streak = 0;

            for (int i = 0; i < seatingChart.Length; i++)
            {
                if (i % 3 == 0)
                { //new row, start over
                    streak = 0;
                }

                if (seatingChart[i] == IsOccupied)
                { //streak continues
                    streak++;
                }
                else
                { //streak is over
                    streak = 0;
                    continue;
                }

                if (streak == 3)
                {
                    fullRows += 1;
                }
            }

            return fullRows;
        }
    }
}
