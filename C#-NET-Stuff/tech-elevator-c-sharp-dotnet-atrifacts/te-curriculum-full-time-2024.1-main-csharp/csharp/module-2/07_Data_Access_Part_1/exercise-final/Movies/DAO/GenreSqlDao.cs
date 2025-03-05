using Movies.Models;
using System;
using System.Data.SqlClient;
using System.Collections.Generic;

namespace Movies.DAO
{
    public class GenreSqlDao : IGenreDao
    {
        private readonly string connectionString;

        private readonly string GENRE_SELECT = "SELECT g.genre_id, g.genre_name FROM genre g ";

        public GenreSqlDao(string connString)
        {
            connectionString = connString;
        }

        public Genre GetGenreById(int id)
        {
            Genre genre = null;

            using (SqlConnection conn = new SqlConnection(connectionString))
            {
                conn.Open();

                SqlCommand cmd = new SqlCommand(GENRE_SELECT + " WHERE g.genre_id = @genre_id", conn);
                cmd.Parameters.AddWithValue("@genre_id", id);
                SqlDataReader reader = cmd.ExecuteReader();

                if (reader.Read())
                {
                    genre = MapRowToGenre(reader);
                }
            }

            return genre;
        }

        public List<Genre> GetGenres()
        {
            List<Genre> genres = new List<Genre>();

            using (SqlConnection conn = new SqlConnection(connectionString))
            {
                conn.Open();

                SqlCommand cmd = new SqlCommand(GENRE_SELECT, conn);
                SqlDataReader reader = cmd.ExecuteReader();

                while (reader.Read())
                {
                    Genre genre = MapRowToGenre(reader);
                    genres.Add(genre);
                }
            }

            return genres;
        }

        public List<Genre> GetGenresByName(string name, bool useWildCard)
        {
            List<Genre> genres = new List<Genre>();

            if (useWildCard)
            {
                name = "%" + name + "%";
            }

            string sqlGetGenres = GENRE_SELECT + " WHERE g.genre_name LIKE @name";
            using (SqlConnection conn = new SqlConnection(connectionString))
            {
                conn.Open();
             
                SqlCommand cmd = new SqlCommand(sqlGetGenres, conn);
                cmd.Parameters.AddWithValue("@name", name);

                SqlDataReader reader = cmd.ExecuteReader();
                while (reader.Read())
                {
                    Genre genre = MapRowToGenre(reader);
                    genres.Add(genre);
                }
            }

            return genres;
        }


        private Genre MapRowToGenre(SqlDataReader reader)
        {
            Genre genre = new Genre();
            genre.Id = Convert.ToInt32(reader["genre_id"]);
            genre.Name = Convert.ToString(reader["genre_name"]);

            return genre;
        }
    }
}
