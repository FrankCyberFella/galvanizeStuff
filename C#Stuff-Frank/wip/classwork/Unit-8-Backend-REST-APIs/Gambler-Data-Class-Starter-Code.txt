       /***********************************************************************
       * Gambler Data Model Starter Code
       ***********************************************************************/
       
       // Data members

       public int        Id        { get; set; }
       public String     Name      { get; set; }
       public String     Address   { get; set; }
       public double     Salary    { get; set; }
       public DateTime   BirthDate { get; set; }

       // 0-arg constructor may be used by C# when needed
       public Gambler() { }

       // 5-arg constructor
       public Gambler(int id, string name, string address, double salary, DateTime birthDate)
       {
           Id        = id;
           Name      = name;
           Address   = address;
           Salary    = salary;
           BirthDate = birthDate;
       }
   }