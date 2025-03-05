using Movies.Models;
using System;
using System.Collections.Generic;
using System.Data.SqlClient;

namespace Movies.DAO
{
    public class PersonSqlDao : IPersonDao
    {
        private readonly string connectionString;

        private readonly string PERSON_SELECT = "SELECT p.person_id, p.person_name, p.birthday, " +
        "p.deathday, p.biography, p.profile_path, p.home_page FROM person p ";

        public PersonSqlDao(string connString)
        {
            connectionString = connString;
        }

        public Person GetPersonById(int id)
        {
            Person person = null;

            using (SqlConnection conn = new SqlConnection(connectionString))
            {
                conn.Open();

                SqlCommand cmd = new SqlCommand(PERSON_SELECT + " WHERE p.person_id = @person_id", conn);
                cmd.Parameters.AddWithValue("@person_id", id);
                SqlDataReader reader = cmd.ExecuteReader();

                if (reader.Read())
                {
                    person = MapRowToPerson(reader);
                }
            }

            return person;
        }

        public List<Person> GetPersons()
        {
            List<Person> persons = new List<Person>();

            using (SqlConnection conn = new SqlConnection(connectionString))
            {
                conn.Open();

                SqlCommand cmd = new SqlCommand(PERSON_SELECT, conn);
                SqlDataReader reader = cmd.ExecuteReader();

                while (reader.Read())
                {
                    Person person = MapRowToPerson(reader);
                    persons.Add(person);
                }
            }

            return persons;
        }

        public List<Person> GetPersonsByCollectionName(string collectionName, bool useWildCard)
        {
            List<Person> persons = new List<Person>();
            if (useWildCard)
            {
                collectionName = "%" + collectionName + "%";
            }

            string sqlGetPersons = PERSON_SELECT + " JOIN movie_actor ma ON p.person_id = ma.actor_id " +
                    "JOIN movie m ON ma.movie_id = m.movie_id " +
                    "JOIN collection c ON m.collection_id = c.collection_id " +
                    "WHERE c.collection_name LIKE @collectionName " +
                    "ORDER BY p.person_name";
            using (SqlConnection conn = new SqlConnection(connectionString))
            {
                conn.Open();

                SqlCommand cmd = new SqlCommand(sqlGetPersons, conn);
                cmd.Parameters.AddWithValue("@collectionName", collectionName);

                SqlDataReader reader = cmd.ExecuteReader();
                while (reader.Read())
                {
                    Person person = MapRowToPerson(reader);
                    persons.Add(person);
                }
            }

            return persons;
        }

        public List<Person> GetPersonsByName(string name, bool useWildCard)
        {
            List<Person> persons = new List<Person>();

            if (useWildCard)
            {
                name = "%" + name + "%";
            }

            string sqlGetPersons = PERSON_SELECT + " WHERE p.person_name LIKE @name";
            using (SqlConnection conn = new SqlConnection(connectionString))
            {
                conn.Open();

                SqlCommand cmd = new SqlCommand(sqlGetPersons, conn);
                cmd.Parameters.AddWithValue("@name", name);

                SqlDataReader reader = cmd.ExecuteReader();
                while (reader.Read())
                {
                    Person person = MapRowToPerson(reader);
                    persons.Add(person);
                }
            }

            return persons;
        }

        private Person MapRowToPerson(SqlDataReader reader)
        {
            Person person = new Person();
            person.Id = Convert.ToInt32(reader["person_id"]);
            person.Name = Convert.ToString(reader["person_name"]);
            person.Birthday = SqlUtil.NullableDateTime(reader["birthday"]);
            person.DeathDate = SqlUtil.NullableDateTime(reader["deathday"]);
            person.Biography = SqlUtil.NullableString(reader["biography"]);
            person.ProfilePath = SqlUtil.NullableString(reader["profile_path"]);
            person.HomePage = SqlUtil.NullableString(reader["home_page"]);

            return person;
        }
    }
}
