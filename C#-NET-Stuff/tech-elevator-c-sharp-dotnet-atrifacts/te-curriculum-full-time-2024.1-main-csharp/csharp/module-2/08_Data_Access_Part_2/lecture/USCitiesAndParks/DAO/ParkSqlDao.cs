using System;
using System.Collections.Generic;
using System.Data.SqlClient;
using USCitiesAndParks.Exceptions;
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
            int count = 0;
            string sql = "SELECT COUNT(*) AS count FROM park;";
            using (SqlConnection conn = new SqlConnection(connectionString))
            {
                conn.Open();

                SqlCommand cmd = new SqlCommand(sql, conn);
                SqlDataReader reader = cmd.ExecuteReader();

                if (reader.Read())
                {
                    count = Convert.ToInt32(reader["count"]);
                }
            }
            return count;
        }

        public IList<string> GetParkNames()
        {
            List<string> parkNames = new List<string>();
            string sql = "SELECT park_name FROM park ORDER by park_name ASC;";
            using (SqlConnection conn = new SqlConnection(connectionString))
            {
                conn.Open();
                SqlCommand cmd = new SqlCommand(sql, conn);
                SqlDataReader reader = cmd.ExecuteReader();
                while (reader.Read())
                {
                    string parkName = Convert.ToString(reader["park_name"]);
                    parkNames.Add(parkName);
                }
            }
            return parkNames;
        }

        public Park GetRandomPark()
        {
            Park park = null;
            string sql = "SELECT TOP 1 * FROM park ORDER BY NEWID();";
            using (SqlConnection conn = new SqlConnection(connectionString))
            {
                conn.Open();
                SqlCommand cmd = new SqlCommand(sql, conn);
                SqlDataReader reader = cmd.ExecuteReader();
                if (reader.Read())
                {
                    park = MapRowToPark(reader);
                }
            }
            return park;
        }

        public IList<Park> GetParksWithCamping()
        {
            IList<Park> parks = new List<Park>();
            string sql = "SELECT park_id, park_name, date_established, area, has_camping " +
                         "FROM park WHERE has_camping = 1;";
            using (SqlConnection conn = new SqlConnection(connectionString))
            {
                conn.Open();
                SqlCommand cmd = new SqlCommand(sql, conn);
                SqlDataReader reader = cmd.ExecuteReader();
                while (reader.Read())
                {
                    Park park = MapRowToPark(reader);
                    parks.Add(park);
                }
            }
            return parks;
        }

        public Park GetParkById(int parkId)
        {
            Park park = null;
            string sql = "SELECT park_id, park_name, date_established, area, has_camping FROM park WHERE park_id = @park_id;";
            using (SqlConnection conn = new SqlConnection(connectionString))
            {
                conn.Open();
                SqlCommand cmd = new SqlCommand(sql, conn);
                cmd.Parameters.AddWithValue("@park_id", parkId);
                SqlDataReader reader = cmd.ExecuteReader();
                if (reader.Read())
                {
                    park = MapRowToPark(reader);
                }
            }
            return park;
        }

        public IList<Park> GetParksByState(string stateAbbreviation)
        {
            IList<Park> parks = new List<Park>();
            string sql = "SELECT p.park_id, park_name, date_established, area, has_camping FROM park p " +
                         "JOIN park_state ps ON p.park_id = ps.park_id WHERE state_abbreviation = @state_abbreviation;";
            using (SqlConnection conn = new SqlConnection(connectionString))
            {
                conn.Open();
                SqlCommand cmd = new SqlCommand(sql, conn);
                cmd.Parameters.AddWithValue("@state_abbreviation", stateAbbreviation);
                SqlDataReader reader = cmd.ExecuteReader();
                while (reader.Read())
                {
                    Park park = MapRowToPark(reader);
                    parks.Add(park);
                }
            }
            return parks;
        }

        public Park CreatePark(Park park)
        {
            throw new DaoException("CreatePark() not implemented");
        }

        public Park UpdatePark(Park park)
        {
            throw new DaoException("UpdatePark() not implemented");
        }

        public int DeleteParkById(int parkId)
        {
            throw new DaoException("DeleteParkById() not implemented");
        }

        public void LinkParkState(int parkId, string stateAbbreviation)
        {
            throw new DaoException("LinkParkState() not implemented");
        }

        private Park MapRowToPark(SqlDataReader reader)
        {
            Park park = new Park();
            park.ParkId = Convert.ToInt32(reader["park_id"]);
            park.ParkName = Convert.ToString(reader["park_name"]);
            park.DateEstablished = Convert.ToDateTime(reader["date_established"]);
            park.Area = Convert.ToDecimal(reader["area"]);
            park.HasCamping = Convert.ToBoolean(reader["has_camping"]);

            return park;
        }
    }
}
