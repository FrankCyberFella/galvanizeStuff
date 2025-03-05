using System.ComponentModel.DataAnnotations.Schema;
using System.Text.Json.Serialization;

namespace GamblerAPI.Model
{
    public class Gambler
    {
        /***********************************************************************
         * Gambler Data Model for use with Entity Fraework
         * 
         * Describes data being transfered to and from the Server
         ***********************************************************************/

        // Data members with Entity Framework notations
        // Use of [JsonPropertyName] is required when the class data member name
        //       does not match the associated table column names
        //
        // Use of [DatabaseGenerated(DatabaseGeneratedOption.Identity)] tell Entity Framework
        //        the Data Base manager will generate this, so don't expect it when we add a new entry

        [JsonPropertyName("id")]                 // Relate JSON property to object property since names differ
        [DatabaseGenerated(DatabaseGeneratedOption.Identity)]  // This is a Generated value - Identity column
        public int      Id        { get; set; }

        [JsonPropertyName("gambler_name")]       // Relate JSON property to object property since names differ
        public string   Name      { get; set; }

        // ? next to variable data type indicates it could be null to C#
        [JsonPropertyName("address")]             // Relate JSON property to object property since names differ
        public string?  Address   { get; set; }

        [JsonPropertyName("monthly_salary")]      // Relate JSON property to object property since names differ
        public double   Salary    { get; set; }

        [JsonPropertyName("birth_date")]          // Relate JSON property to object property since names differ
        public DateTime BirthDate { get; set; }


        // 0-arg constructor may be used by C# when needed
        public Gambler() { }

        // 5-arg constructor
        public Gambler(int id, string name, string address, double salary, DateTime birthDate)
        {
            Id = id;
            Name = name;
            Address = address;
            Salary = salary;
            BirthDate = birthDate;
        }

        /**************************************************************************************************
         * These overrides are not required for Entity Framework, but may also be found in a class using
         *       Entity Framework
         **************************************************************************************************/ 
        public override bool Equals(object? obj)
        {
            return obj is Gambler gambler &&
                   Id == gambler.Id &&
                   Name == gambler.Name &&
                   Address == gambler.Address &&
                   Salary == gambler.Salary &&
                   BirthDate == gambler.BirthDate;
        }

        public override int GetHashCode()
        {
            return base.GetHashCode();
        }

        public override string? ToString()
        {
            return "------ Gambler Model Object ------ ]"
                  + "\n         Id: " + Id
                  + " \n      Name: " + Name
                  + " \n   Address: " + Address
                  + " \n    Salary: " + Salary
                  + " \nBirth Date: " + BirthDate;
        }
    } // End of class
} // End of namespace
