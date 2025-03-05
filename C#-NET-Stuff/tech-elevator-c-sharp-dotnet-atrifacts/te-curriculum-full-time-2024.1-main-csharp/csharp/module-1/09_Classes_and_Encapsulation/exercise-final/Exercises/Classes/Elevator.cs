namespace Exercises.Classes
{
    public class Elevator
    {
        /// <summary>
        /// Current elevator level
        /// </summary>
        public int CurrentLevel { get; private set; } = 1;


        /// <summary>
        /// Number of levels available for elevator.
        /// </summary>
        public int NumberOfLevels { get; private set; }


        /// <summary>
        /// Is Elevator Door Open?
        /// </summary>
        public bool DoorIsOpen { get; private set; }


        /// <summary>
        /// Creates a new elevator
        /// </summary>
        /// <param name="totalNumberOfFloors">Number of floors in the elevator</param>
        public Elevator(int totalNumberOfFloors)
        {
            this.NumberOfLevels = totalNumberOfFloors;
        }

        /// <summary>
        /// Opens the elevator door
        /// </summary>
        public void OpenDoor()
        {
            DoorIsOpen = true;
        }

        /// <summary>
        /// Closes the elevator door
        /// </summary>
        public void CloseDoor()
        {
            DoorIsOpen = false;
        }

        /// <summary>
        /// Moves the elevator up, as long as the door is closed and the desired floor is not past the last floor
        /// </summary>
        /// <param name="desiredFloor">Desired floor to go to</param>
        public void GoUp(int desiredFloor)
        {
            if (!DoorIsOpen && desiredFloor > CurrentLevel && desiredFloor <= NumberOfLevels)
            {
                CurrentLevel = desiredFloor;
            }
        }

        /// <summary>
        /// Moves the elevator down, as long as the door is closed and desired floor is lower than current but not past 1
        /// </summary>
        /// <param name="desiredFloor">Floor to go to</param>
        public void GoDown(int desiredFloor)
        {
            if (!DoorIsOpen && desiredFloor < CurrentLevel && desiredFloor > 0)
            {
                CurrentLevel = desiredFloor;
            }
        }
    }
}
