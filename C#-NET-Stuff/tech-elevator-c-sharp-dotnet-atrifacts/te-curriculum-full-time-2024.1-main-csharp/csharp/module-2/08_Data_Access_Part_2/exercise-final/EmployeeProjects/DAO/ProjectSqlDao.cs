using EmployeeProjects.Exceptions;
using EmployeeProjects.Models;
using System;
using System.Collections.Generic;
using System.Data.SqlClient;

namespace EmployeeProjects.DAO
{
    public class ProjectSqlDao : IProjectDao
    {
        private readonly string connectionString;

        public ProjectSqlDao(string connString)
        {
            connectionString = connString;
        }

        public Project GetProjectById(int projectId)
        {
            Project project = null;
            string sql = "SELECT project_id, name, from_date, to_date FROM project WHERE project_id = @project_id;";
            try
            {
                using (SqlConnection conn = new SqlConnection(connectionString))
                {
                    conn.Open();

                    SqlCommand cmd = new SqlCommand(sql, conn);
                    cmd.Parameters.AddWithValue("@project_id", projectId);

                    SqlDataReader reader = cmd.ExecuteReader();

                    if (reader.Read())
                    {
                        project = MapRowToProject(reader);
                    }
                }
            }
            catch (SqlException ex)
            {
                throw new DaoException("Error getting project", ex);
            }

            return project;
        }

        public List<Project> GetProjects()
        {
            List<Project> projects = new List<Project>();
            string sql = "SELECT project_id, name, from_date, to_date FROM project;";
            try
            {
                using (SqlConnection conn = new SqlConnection(connectionString))
                {
                    conn.Open();

                    SqlCommand cmd = new SqlCommand(sql, conn);
                    SqlDataReader reader = cmd.ExecuteReader();

                    while (reader.Read())
                    {
                        Project project = MapRowToProject(reader);
                        projects.Add(project);
                    }
                }
            }
            catch (SqlException ex)
            {
                throw new DaoException("Error getting projects", ex);
            }

            return projects;
        }

        public Project CreateProject(Project newProject)
        {
            int newProjectId;
            string sql = @"INSERT INTO project(name, from_date, to_date) 
                           OUTPUT INSERTED.project_id 
                           VALUES (@name, @from_date, @to_date);";
            try
            {
                using (SqlConnection conn = new SqlConnection(connectionString))
                {
                    conn.Open();

                    SqlCommand cmd = new SqlCommand(sql, conn);
                    cmd.Parameters.AddWithValue("@name", newProject.Name);
                    cmd.Parameters.AddWithValue("@from_date", ((object)newProject.FromDate) ?? DBNull.Value);
                    cmd.Parameters.AddWithValue("@to_date", ((object)newProject.ToDate) ?? DBNull.Value);

                    newProjectId = Convert.ToInt32(cmd.ExecuteScalar());
                }
            }
            catch (SqlException ex)
            {
                throw new DaoException("Error creating project", ex);
            }
            
            return GetProjectById(newProjectId);
        }

        public void LinkProjectEmployee(int projectId, int employeeId)
        {
            string sql = "INSERT INTO project_employee (project_id, employee_id) VALUES (@project_id, @employee_id);";
            try
            {
                using (SqlConnection conn = new SqlConnection(connectionString))
                {
                    conn.Open();

                    SqlCommand cmd = new SqlCommand(sql, conn);
                    cmd.Parameters.AddWithValue("@project_id", projectId);
                    cmd.Parameters.AddWithValue("@employee_id", employeeId);

                    cmd.ExecuteNonQuery();
                }
            }
            catch (SqlException ex)
            {
                throw new DaoException("Error linking project and employee", ex);
            }
        }

        public void UnlinkProjectEmployee(int projectId, int employeeId)
        {
            string sql = "DELETE FROM project_employee WHERE project_id = @project_id AND employee_id = @employee_id;";
            try
            {
                using (SqlConnection conn = new SqlConnection(connectionString))
                {
                    conn.Open();

                    SqlCommand cmd = new SqlCommand(sql, conn);
                    cmd.Parameters.AddWithValue("@project_id", projectId);
                    cmd.Parameters.AddWithValue("@employee_id", employeeId);

                    cmd.ExecuteNonQuery();
                }
            }
            catch (SqlException ex)
            {
                throw new DaoException("Error unassinging employee from project", ex);
            }
        }

        public Project UpdateProject(Project project)
        {
            string sql = @"UPDATE project SET name = @name, from_date = @from_date, 
                           to_date = @to_date WHERE project_id = @project_id";
            try
            {
                using (SqlConnection conn = new SqlConnection(connectionString))
                {
                    conn.Open();

                    SqlCommand cmd = new SqlCommand(sql, conn);
                    cmd.Parameters.AddWithValue("@name", project.Name);
                    cmd.Parameters.AddWithValue("@from_date", ((object)project.FromDate) ?? DBNull.Value);
                    cmd.Parameters.AddWithValue("@to_date", ((object)project.ToDate) ?? DBNull.Value);
                    cmd.Parameters.AddWithValue("@project_id", project.ProjectId);

                    int numberOfRows = cmd.ExecuteNonQuery();
                    if (numberOfRows == 0)
                    {
                        throw new DaoException("Zero rows affected, expected at least one");
                    }
                }
            }
            catch (SqlException ex)
            {
                throw new DaoException("Error updating project", ex);
            }
            
            return GetProjectById(project.ProjectId);
        }

        public int DeleteProjectById(int projectId)
        {
            string sql = "DELETE FROM project_employee WHERE project_id = @project_id;";
            try
            {
                using (SqlConnection conn = new SqlConnection(connectionString))
                {
                    conn.Open();

                    SqlCommand cmd = new SqlCommand(sql, conn);
                    cmd.Parameters.AddWithValue("@project_id", projectId);
                    cmd.ExecuteNonQuery();

                    cmd = new SqlCommand("DELETE FROM project WHERE project_id = @project_id;", conn);
                    cmd.Parameters.AddWithValue("@project_id", projectId);

                    return cmd.ExecuteNonQuery();
                }
            }
            catch (SqlException ex)
            {
                throw new DaoException("Error deleting project", ex);
            }
        }

        private Project MapRowToProject(SqlDataReader reader)
        {
            Project project = new Project();
            project.ProjectId = Convert.ToInt32(reader["project_id"]);
            project.Name = Convert.ToString(reader["name"]);
            if (reader["from_date"] is DBNull)
            {
                project.FromDate = null;
            }
            else
            {
                project.FromDate = Convert.ToDateTime(reader["from_date"]);
            }
            if (reader["to_date"] is DBNull)
            {
                project.ToDate = null;
            }
            else
            {
                project.ToDate = Convert.ToDateTime(reader["to_date"]);
            }

            return project;
        }
    }
}
