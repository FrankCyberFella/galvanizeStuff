namespace Exercises.Classes
{
    public class Airplane
    {
        /// <summary>
        /// 6-Character Plane Number
        /// </summary>
        public string PlaneNumber { get; private set; }


        /// <summary>
        /// Number of already booked first class seats
        /// </summary>
        public int BookedFirstClassSeats { get; private set; }


        /// <summary>
        /// Available number of first class seats
        /// </summary>
        public int AvailableFirstClassSeats
        {
            get { return TotalFirstClassSeats - BookedFirstClassSeats; }
        }

        /// <summary>
        /// Total number of first class seats
        /// </summary>
        public int TotalFirstClassSeats { get; private set; }


        /// <summary>
        /// Number of already booked coach seats
        /// </summary>
        public int BookedCoachSeats { get; private set; }


        /// <summary>
        /// Available number of coach seats
        /// </summary>
        public int AvailableCoachSeats
        {
            get { return TotalCoachSeats - BookedCoachSeats; }
        }

        /// <summary>
        /// Total number of coach seats
        /// </summary>
        public int TotalCoachSeats { get; private set; }


        /// <summary>
        /// Creates a new airplane
        /// </summary>
        /// <param name="planeNumber">Plane number</param>
        /// <param name="totalFirstClassSeats">Total number of first class seats that can be booked</param>
        /// <param name="totalCoachSeats">Total number of coach seats that can be booked</param>
        public Airplane(string planeNumber, int totalFirstClassSeats, int totalCoachSeats)
        {
            this.PlaneNumber = planeNumber;
            this.TotalFirstClassSeats = totalFirstClassSeats;
            this.TotalCoachSeats = totalCoachSeats;
        }

        /// <summary>
        /// Reserves a first class or coach seat
        /// </summary>
        /// <param name="firstClass">True if the reservation is for first class, false for coach</param>
        /// <param name="totalNumberOfSeats">Total number of seats to reserve</param>
        /// <returns>True if reservation was successful, false otherwise</returns>
        public bool ReserveSeats(bool firstClass, int totalNumberOfSeats)
        {
            if (firstClass)
            {
                if (totalNumberOfSeats > AvailableFirstClassSeats)
                {
                    return false;
                }

                BookedFirstClassSeats += totalNumberOfSeats;
            }
            else
            {
                if (totalNumberOfSeats > AvailableCoachSeats)
                {
                    return false;
                }

                BookedCoachSeats += totalNumberOfSeats;
            }

            return true;
        }
    }
}
