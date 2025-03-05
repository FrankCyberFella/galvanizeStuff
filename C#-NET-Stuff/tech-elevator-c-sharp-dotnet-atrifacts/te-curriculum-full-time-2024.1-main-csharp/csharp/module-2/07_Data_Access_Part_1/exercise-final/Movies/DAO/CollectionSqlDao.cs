using Movies.Models;
using System;
using System.Collections.Generic;
using System.Data.SqlClient;

namespace Movies.DAO
{
    public class CollectionSqlDao : ICollectionDao
    {
        private readonly string connectionString;

        private readonly string COLLECTION_SELECT = "SELECT c.collection_id, c.collection_name FROM collection c ";

        public CollectionSqlDao(string connString)
        {
            connectionString = connString;
        }

        public Collection GetCollectionById(int id)
        {
            Collection collection = null;

            using (SqlConnection conn = new SqlConnection(connectionString))
            {
                conn.Open();

                SqlCommand cmd = new SqlCommand(COLLECTION_SELECT + " WHERE c.collection_id = @collection_id", conn);
                cmd.Parameters.AddWithValue("@collection_id", id);
                SqlDataReader reader = cmd.ExecuteReader();

                if (reader.Read())
                {
                    collection = MapRowToCollection(reader);
                }
            }

            return collection;
        }

        public List<Collection> GetCollections()
        {
            List<Collection> collections = new List<Collection>();

            using (SqlConnection conn = new SqlConnection(connectionString))
            {
                conn.Open();

                SqlCommand cmd = new SqlCommand(COLLECTION_SELECT, conn);
                SqlDataReader reader = cmd.ExecuteReader();

                while (reader.Read())
                {
                    Collection collection = MapRowToCollection(reader);
                    collections.Add(collection);
                }
            }

            return collections;
        }

        public List<Collection> GetCollectionsByName(string name, bool useWildCard)
        {
            List<Collection> collections = new List<Collection>();

            if (useWildCard)
            {
                name = "%" + name + "%";
            }

            string sqlGetCollections = COLLECTION_SELECT + " WHERE c.collection_name LIKE @name";
            using (SqlConnection conn = new SqlConnection(connectionString))
            {
                conn.Open();

                SqlCommand cmd = new SqlCommand(sqlGetCollections, conn);
                cmd.Parameters.AddWithValue("@name", name);

                SqlDataReader reader = cmd.ExecuteReader();
                while (reader.Read())
                {
                    Collection collection = MapRowToCollection(reader);
                    collections.Add(collection);
                }
            }

            return collections;
        }

        private Collection MapRowToCollection(SqlDataReader reader)
        {
            Collection collection = new Collection();
            collection.Id = Convert.ToInt32(reader["collection_id"]);
            collection.Name = Convert.ToString(reader["collection_name"]);

            return collection;
        }
    }
}
