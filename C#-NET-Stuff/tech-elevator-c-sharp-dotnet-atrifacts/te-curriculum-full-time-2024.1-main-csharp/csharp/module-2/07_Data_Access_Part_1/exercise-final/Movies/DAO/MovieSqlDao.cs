using Movies.Models;
using System;
using System.Collections.Generic;
using System.Data.SqlClient;

namespace Movies.DAO
{
    public class MovieSqlDao : IMovieDao
    {
        private readonly string connectionString;

        private readonly string MOVIE_SELECT = "SELECT m.movie_id, m.title, m.overview, m.tagline, " +
            "m.poster_path, m.home_page, m.release_date, m.length_minutes, m.director_id, " +
            "m.collection_id FROM movie m ";

        public MovieSqlDao(string connString)
        {
            connectionString = connString;
        }

        public Movie GetMovieById(int id)
        {
            Movie movie = null;

            using (SqlConnection conn = new SqlConnection(connectionString))
            {
                conn.Open();

                SqlCommand cmd = new SqlCommand(MOVIE_SELECT + " WHERE m.movie_id = @movie_id", conn);
                cmd.Parameters.AddWithValue("@movie_id", id);
                SqlDataReader reader = cmd.ExecuteReader();

                if (reader.Read())
                {
                    movie = MapRowToMovie(reader);
                }
            }

            return movie;
        }

        public List<Movie> GetMovies()
        {
            List<Movie> movies = new List<Movie>();

            using (SqlConnection conn = new SqlConnection(connectionString))
            {
                conn.Open();

                SqlCommand cmd = new SqlCommand(MOVIE_SELECT, conn);
                SqlDataReader reader = cmd.ExecuteReader();

                while (reader.Read())
                {
                    Movie movie = MapRowToMovie(reader);
                    movies.Add(movie);
                }
            }

            return movies;
        }

        public List<Movie> GetMoviesByDirectorNameAndBetweenYears(string directorName, int startYear, int endYear, bool useWildCard)
        {
            List<Movie> movies = new List<Movie>();

            if (useWildCard)
            {
                directorName = "%" + directorName + "%";
            }

            // Adjust start and end dates to force years to be inclusively "between"
            DateTime startDate = new DateTime(startYear - 1, 12, 31);
            DateTime endDate = new DateTime(endYear + 1, 1, 1);

            string sqlGetMovies = MOVIE_SELECT + "JOIN person p ON m.director_id = p.person_id " +
                    "WHERE p.person_name LIKE @director_name " +
                    "AND m.release_date BETWEEN @start_date AND @end_date " +
                    "ORDER BY m.release_date";

            using (SqlConnection conn = new SqlConnection(connectionString))
            {
                conn.Open();

                SqlCommand cmd = new SqlCommand(sqlGetMovies, conn);
                cmd.Parameters.AddWithValue("@director_name", directorName);
                cmd.Parameters.AddWithValue("@start_date", startDate.Date);
                cmd.Parameters.AddWithValue("@end_date", endDate.Date);

                SqlDataReader reader = cmd.ExecuteReader();
                while (reader.Read())
                {
                    Movie movie = MapRowToMovie(reader);
                    movies.Add(movie);
                }
            }

            return movies;
        }

        public List<Movie> GetMoviesByTitle(string title, bool useWildCard)
        {
            List<Movie> movies = new List<Movie>();
            
            if (useWildCard)
            {
                title = "%" + title + "%";
            }

            string sqlGetMovies = MOVIE_SELECT + " WHERE m.title LIKE @title";
            using (SqlConnection conn = new SqlConnection(connectionString))
            {
                conn.Open();

                SqlCommand cmd = new SqlCommand(sqlGetMovies, conn);
                cmd.Parameters.AddWithValue("@title", title);

                SqlDataReader reader = cmd.ExecuteReader();
                while (reader.Read())
                {
                    Movie movie = MapRowToMovie(reader);
                    movies.Add(movie);
                }
            }

            return movies;
        }

        private Movie MapRowToMovie(SqlDataReader reader)
        {
            Movie movie = new Movie();
            movie.Id = Convert.ToInt32(reader["movie_id"]);
            movie.Title = Convert.ToString(reader["title"]);
            movie.Overview = SqlUtil.NullableString(reader["overview"]);
            movie.Tagline = SqlUtil.NullableString(reader["tagline"]);
            movie.PosterPath = SqlUtil.NullableString(reader["poster_path"]);
            movie.HomePage = SqlUtil.NullableString(reader["home_page"]);
            movie.ReleaseDate = SqlUtil.NullableDateTime(reader["release_date"]);
            movie.LengthMinutes = SqlUtil.NullableInt(reader["length_minutes"]);
            movie.DirectorId = SqlUtil.NullableInt(reader["director_id"]);
            movie.CollectionId = SqlUtil.NullableInt(reader["collection_id"]);
            return movie;
        }
    }
}
