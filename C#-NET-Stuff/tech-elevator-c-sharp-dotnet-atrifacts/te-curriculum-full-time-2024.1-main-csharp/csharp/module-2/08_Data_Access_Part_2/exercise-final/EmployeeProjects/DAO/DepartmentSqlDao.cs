using EmployeeProjects.Exceptions;
using EmployeeProjects.Models;
using System;
using System.Collections.Generic;
using System.Data.SqlClient;

namespace EmployeeProjects.DAO
{
    public class DepartmentSqlDao : IDepartmentDao
    {
        private readonly string connectionString;

        public DepartmentSqlDao(string connString)
        {
            connectionString = connString;
        }

        public Department GetDepartmentById(int departmentId)
        {
            Department department = null;
            string sql = @"SELECT department_id, name FROM department 
                           WHERE department_id = @department_id;";

            try
            {
                using (SqlConnection conn = new SqlConnection(connectionString))
                {
                    conn.Open();

                    SqlCommand cmd = new SqlCommand(sql, conn);
                    cmd.Parameters.AddWithValue("@department_id", ((object)departmentId ?? DBNull.Value));

                    SqlDataReader reader = cmd.ExecuteReader();

                    if (reader.Read())
                    {
                        department = MapRowToDepartment(reader);
                    }
                }
            }
            catch (SqlException ex)
            {
                throw new DaoException("Error getting department", ex);
            }

            return department;
        }

        public List<Department> GetDepartments()
        {
            List<Department> departments = new List<Department>();
            string sql = "SELECT department_id, name FROM department;";

            try
            {
                using (SqlConnection conn = new SqlConnection(connectionString))
                {
                    conn.Open();

                    SqlCommand cmd = new SqlCommand(sql, conn);

                    SqlDataReader reader = cmd.ExecuteReader();

                    while (reader.Read())
                    {
                        Department department = MapRowToDepartment(reader);
                        departments.Add(department);
                    }
                }
            }
            catch (SqlException ex)
            {
                throw new DaoException("Error getting departments", ex);
            }

            return departments;
        }

        public Department CreateDepartment(Department department)
        {
            int departmentId;
            string sql = @"INSERT INTO department(name)
                           OUTPUT INSERTED.department_id
                           VALUES (@name);";

            try
            {
                using (SqlConnection conn = new SqlConnection(connectionString))
                {
                    conn.Open();

                    SqlCommand cmd = new SqlCommand(sql, conn);
                    cmd.Parameters.AddWithValue("@name", department.Name);

                    departmentId = (int)cmd.ExecuteScalar();

                }
                
            }
            catch (SqlException ex)
            {
                throw new DaoException("Error creating department", ex);
            }
            
            return GetDepartmentById(departmentId);
        }

        public Department UpdateDepartment(Department department)
        {
            string sql = @"UPDATE department SET name = @name 
                           WHERE department_id = @department_id;";
            try
            {
                using (SqlConnection conn = new SqlConnection(connectionString))
                {
                    conn.Open();

                    SqlCommand cmd = new SqlCommand(sql, conn);
                    cmd.Parameters.AddWithValue("@name", department.Name);
                    cmd.Parameters.AddWithValue("@department_id", department.DepartmentId);

                    int numberOfRows = cmd.ExecuteNonQuery();
                    if (numberOfRows == 0)
                    {
                        throw new DaoException("Zero rows affected, expected at least one");
                    }
                }
            }
            catch (SqlException ex)
            {
                throw new DaoException("Error updating department", ex);
            }
            
            return GetDepartmentById(department.DepartmentId);
        }

        public int DeleteDepartmentById(int id)
        {
            string employeeSql = "UPDATE employee SET department_id = 0 WHERE department_id = @department_id";

            string departmentSql = @"DELETE FROM department 
                           WHERE department_id = @department_id;";

            try
            {
                using (SqlConnection conn = new SqlConnection(connectionString))
                {
                    conn.Open();
                    SqlCommand cmd = new SqlCommand(employeeSql, conn);
                    cmd.Parameters.AddWithValue("@department_id", ((object)id ?? DBNull.Value));

                    cmd.ExecuteNonQuery();

                    cmd = new SqlCommand(departmentSql, conn);
                    cmd.Parameters.AddWithValue("@department_id", id);

                    return cmd.ExecuteNonQuery();
                }
            }
            catch (SqlException ex)
            {
                throw new DaoException("Error deleting department", ex);
            }
        }

        private Department MapRowToDepartment(SqlDataReader reader)
        {
            Department department = new Department();
            department.DepartmentId = Convert.ToInt32(reader["department_id"]);
            department.Name = Convert.ToString(reader["name"]);

            return department;
        }
    }
}
