using static System.Runtime.InteropServices.JavaScript.JSType;

namespace FirstAPI.Model
{
    public class GamblerDao
    {
        // This class handles all data reated activity for teh Gambler data 

        /************************************************************************
        * GamblerDAO Starter Code
        *
        * DAO - Data Access Object - Used to Manipulate data in a data source
        **************************************************************************/

        // Data Source - static so it lives as long as the server live
        private static List<Gambler> gamblers;   // Reference to data source

        // Constructor
        public GamblerDao()
        {
            gamblers = new List<Gambler>(); // Instantiate the data source object

            // Initialize the data source
            gamblers.Add(new Gambler(12, "T Judson Smith", "Los Angeles, CA", 1398.65, DateTime.Parse("05/01/1972")));
            gamblers.Add(new Gambler(49, "Dana Imori", "", 7580.50, DateTime.Parse("08/08/1938")));
            gamblers.Add(new Gambler(201, "S.Q. Elle", "Relational, DA", 1000000.01, DateTime.Parse("05/23/1995")));
            gamblers.Add(new Gambler(54, "Neil Bransfield", "Seattle, WA", 5000.00, DateTime.Parse("03/11/1959")));
            gamblers.Add(new Gambler(382, "Stickman Nelson", "Cumberland, MD", 12983.75, DateTime.Parse("10/21/1955")));
            gamblers.Add(new Gambler(572, "Al Mostbroke", "Clayton MO", 4505.55, DateTime.Parse("01/18/1975")));
        }

        // Method to return all entries in the data source
        public List<Gambler> getGamblers()
        {
            return gamblers;
        }

        // Method to return a Gambler with a specific id or NULL if not found
        public Gambler getSpecificGambler(int id)
        {
            // find the Gambler with the id passed to us in the data source
            // .Find() will return the element that matches the condition or null
            return gamblers.Find(g => g.Id == id);
        }

        // Method to return Gamblers by name or partial name
        // Since there may be multiple matches, we return a List instead of one Gambler
        public List<Gambler> getGamblerByName(string aName)
        {
            // .Where returns an IEnumerable type which we must converted to a List
            
            return gamblers.Where(g => g.Name.Contains(aName)).ToList();
        }

        // Method to add a Gambler to the data source
        public Gambler AddAGambler(Gambler aGambler)
        {
            gamblers.Add(aGambler);
            return aGambler;  // return the Gambler we got
        }

    } // End of Class
} // End of namespace
