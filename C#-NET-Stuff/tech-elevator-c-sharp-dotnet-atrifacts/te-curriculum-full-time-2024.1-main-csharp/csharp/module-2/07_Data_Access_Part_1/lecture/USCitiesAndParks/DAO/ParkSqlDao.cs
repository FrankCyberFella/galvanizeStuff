using System;
using System.Collections.Generic;
using System.Data.SqlClient;
using USCitiesAndParks.Models;

namespace USCitiesAndParks.DAO
{
    public class ParkSqlDao : IParkDao
    {
        private readonly string connectionString;

        public ParkSqlDao(string connString)
        {
            connectionString = connString;
        }

        public int GetParkCount()
        {
            return 0;
        }

        public DateTime GetOldestParkDate()
        {
            return DateTime.Now;
        }

        public decimal GetAverageParkArea()
        {
            return 0.0M;
        }

        public IList<string> GetParkNames()
        {
            return new List<string>();
        }

        public Park GetRandomPark()
        {
            return null;
        }

        public IList<Park> GetParksWithCamping()
        {
            return new List<Park>();
        }

        public Park GetParkById(int parkId)
        {
            return null;
        }

        public IList<Park> GetParksByState(string stateAbbreviation)
        {
            return new List<Park>();
        }

        public IList<Park> GetParksByName(string name, bool useWildCard)
        {
            return new List<Park>();
        }

        private Park MapRowToPark(SqlDataReader reader)
        {
            return new Park();
        }
    }
}
