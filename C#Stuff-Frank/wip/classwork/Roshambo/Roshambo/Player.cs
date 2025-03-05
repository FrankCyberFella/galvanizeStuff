namespace Roshambo
{
    public abstract class Player
    {
        // Instance variables in a class should be provide to implement Encapsulation

        private string   name;    // Player name

        // Note: use of class.EnumName to access the enum as a data type
        private Roshambo.RoshamboValues choice;  // Roshambo enum object

        // Define properties to control access to the instance variables, to maintain encapsulation
        // Property, by convention, is the instance varaiable name in PascalCase

        public string   Name { get { return name; } }  // no setter defined, read-only property

        public Roshambo.RoshamboValues Choice
        {
            get { return choice;  }
            set { choice = value; }
        }                          

        // Constructor(s) - initialize objects
        //                  no return type, same name as class, may take parameters

        public Player(string name, Roshambo.RoshamboValues choice)
        {
            // this. is required to indicate the current object if parameters have same name as instance
            this.name   = name;
            this.choice = choice;
        }

        // Abstract - require the subclass to define - we don't know how it should work 
        public abstract Roshambo.RoshamboValues GenerateRoshambo();
    }
}
