/************************************************************************/
* GamblerDAO Starter Code
*
* DAO - Data Access Object - Used to Manipulate data in s data source
**************************************************************************/

// Data Source
private List<Gambler> gamblers;   // Reference to data source

// Constructor
public GamblerDAO()
{
    this.gamblers = new List<Gambler>(); // Instantiate the data source object

    // INitialize tehdata siurce
    gamblers.Add(new Gambler(12 , "T Judson Smith" , "Los Angeles, CA", 1398.65,    DateTime.Parse("05/01/1972")));
    gamblers.Add(new Gambler(49 , "Dana Imori"     , ""               , 7580.50,    DateTime.Parse("08/08/1938")));
    gamblers.Add(new Gambler(201, "S.Q. Elle"      , "Relational, DA" , 1000000.01, DateTime.Parse("05/23/1995")));
    gamblers.Add(new Gambler(54 , "Neil Bransfield", "Seattle, WA"    , 5000.00,    DateTime.Parse("03/11/1959")));
    gamblers.Add(new Gambler(382, "Stickman Nelson", "Cumberland, MD" , 12983.75,   DateTime.Parse("10/21/1955")));
    gamblers.Add(new Gambler(572, "Al Mostbroke"   , "Clayton MO"     , 4505.55,    DateTime.Parse("01/18/1975")));
}

// Method to return all entries in the data source
public List<Gambler> getGamblers()
{
    return gamblers;
}
