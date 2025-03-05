using EmployeeProjects.Exceptions;
using EmployeeProjects.Models;
using System;
using System.Collections.Generic;
using System.Data.SqlClient;

namespace EmployeeProjects.DAO
{
    public class EmployeeSqlDao : IEmployeeDao
    {
        private readonly string connectionString;

        public EmployeeSqlDao(string connString)
        {
            connectionString = connString;
        }

        public Employee GetEmployeeById(int id)
        {
            Employee employee = new Employee();
            string sql = @"SELECT * FROM employee
                           WHERE employee_id = @employee_id";
            try
            {
                using (SqlConnection conn = new SqlConnection(connectionString))
                {
                    conn.Open();

                    SqlCommand cmd = new SqlCommand(sql, conn);
                    cmd.Parameters.AddWithValue("@employee_id", id);

                    SqlDataReader reader = cmd.ExecuteReader();

                    if (reader.Read())
                    {
                        employee = MapRowToEmployee(reader);
                    }
                }
            }
            catch (SqlException ex)
            {
                throw new DaoException("Error getting employee", ex);
            }
            return employee;
        }

        public List<Employee> GetEmployees()
        {
            List<Employee> employees = new List<Employee>();
            string sql = @"SELECT employee_id, department_id, first_name, last_name, birth_date, hire_date FROM employee;";
            try
            {
                using (SqlConnection conn = new SqlConnection(connectionString))
                {
                    conn.Open();

                    SqlCommand cmd = new SqlCommand(sql, conn);
                    SqlDataReader reader = cmd.ExecuteReader();

                    while (reader.Read())
                    {
                        Employee employee = MapRowToEmployee(reader);
                        employees.Add(employee);
                    }
                }
            }
            catch (SqlException ex)
            {
                throw new DaoException("Error getting employees", ex);
            }
            return employees;
        }

        public List<Employee> GetEmployeesByFirstNameLastName(string firstNameSearch, string lastNameSearch)
        {
            List<Employee> employees = new List<Employee>();
            string sql = @"SELECT employee_id, department_id, first_name, last_name, birth_date, hire_date FROM employee 
                           WHERE first_name LIKE @first_name AND last_name LIKE @last_name;";
            try
            {
                using (SqlConnection conn = new SqlConnection(connectionString))
                {
                    conn.Open();

                    SqlCommand cmd = new SqlCommand(sql, conn);
                    cmd.Parameters.AddWithValue("@first_name", "%" + firstNameSearch + "%");
                    cmd.Parameters.AddWithValue("@last_name", "%" + lastNameSearch + "%");

                    SqlDataReader reader = cmd.ExecuteReader();

                    while (reader.Read())
                    {
                        Employee employee = MapRowToEmployee(reader);
                        employees.Add(employee);
                    }
                }
            }
            catch (SqlException ex)
            {
                throw new DaoException("Error getting employees by name", ex);
            }
            return employees;
        }

        public List<Employee> GetEmployeesByProjectId(int projectId)
        {
            List<Employee> employees = new List<Employee>();
            string sql = @"SELECT e.employee_id, department_id, first_name, last_name, birth_date, hire_date FROM employee e 
                           JOIN project_employee pe ON e.employee_id = pe.employee_id 
                           WHERE pe.project_id = @project_id;";
            try
            {
                using (SqlConnection conn = new SqlConnection(connectionString))
                {
                    conn.Open();

                    SqlCommand cmd = new SqlCommand(sql, conn);
                    cmd.Parameters.AddWithValue("@project_id", projectId);

                    SqlDataReader reader = cmd.ExecuteReader();

                    while (reader.Read())
                    {
                        Employee employee = MapRowToEmployee(reader);
                        employees.Add(employee);
                    }
                }
            }
            catch (SqlException ex)
            {
                throw new DaoException("Error getting employees by project", ex);
            }
            return employees;
        }

        public List<Employee> GetEmployeesWithoutProjects()
        {
            List<Employee> employees = new List<Employee>();
            string sql = @"SELECT employee_id, department_id, first_name, last_name, birth_date, hire_date FROM employee 
                           WHERE employee_id NOT IN (SELECT employee_id FROM project_employee);";
            try
            {
                using (SqlConnection conn = new SqlConnection(connectionString))
                {
                    conn.Open();

                    SqlCommand cmd = new SqlCommand(sql, conn);
                    SqlDataReader reader = cmd.ExecuteReader();

                    while (reader.Read())
                    {
                        Employee employee = MapRowToEmployee(reader);
                        employees.Add(employee);
                    }
                }
            }
            catch (SqlException ex)
            {
                throw new DaoException("Error getting employees with no project(s)", ex);
            }
            return employees;
        }

        public Employee CreateEmployee(Employee employee)
        {
            int employeeId;
            string sql = @"INSERT INTO employee (department_id, first_name, last_name, birth_date, hire_date) 
                           OUTPUT INSERTED.employee_id
                           VALUES (@department_id, @first_name, @last_name, @birth_date, @hire_date)";

            try
            {
                using (SqlConnection conn = new SqlConnection(connectionString))
                {
                    conn.Open();

                    SqlCommand cmd = new SqlCommand(sql, conn);
                    cmd.Parameters.AddWithValue("@department_id", ((object)employee.DepartmentId ?? DBNull.Value));
                    cmd.Parameters.AddWithValue("@first_name", employee.FirstName);
                    cmd.Parameters.AddWithValue("@last_name", employee.LastName);
                    cmd.Parameters.AddWithValue("@birth_date", employee.BirthDate);
                    cmd.Parameters.AddWithValue("@hire_date", employee.HireDate);

                    employeeId = (int)cmd.ExecuteScalar();

                }
            }
            catch (SqlException ex)
            {
                throw new DaoException("Error creating employee", ex);
            }
            
            return GetEmployeeById(employeeId);
        }

        public Employee UpdateEmployee(Employee employee)
        {
            string sql = @"UPDATE employee SET department_id = @department_id, first_name = @first_name, 
                           last_name = @last_name, birth_date = @birth_date, hire_date = @hire_date 
                           WHERE employee_id = @employee_id";

            try
            {
                using (SqlConnection conn = new SqlConnection(connectionString))
                {
                    conn.Open();

                    SqlCommand cmd = new SqlCommand(sql, conn);
                    cmd.Parameters.AddWithValue("@department_id", ((object)employee.DepartmentId ?? DBNull.Value));
                    cmd.Parameters.AddWithValue("@first_name", employee.FirstName);
                    cmd.Parameters.AddWithValue("@last_name", employee.LastName);
                    cmd.Parameters.AddWithValue("@birth_date", employee.BirthDate);
                    cmd.Parameters.AddWithValue("@hire_date", employee.HireDate);
                    cmd.Parameters.AddWithValue("@employee_id", employee.EmployeeId);

                    int numberOfRows = cmd.ExecuteNonQuery();
                    if (numberOfRows == 0)
                    {
                        throw new DaoException("Zero rows affected, expected at least one");
                    }
                }
            }
            catch (SqlException ex)
            {
                throw new DaoException("Error updating employee", ex);
            }
            
            return GetEmployeeById(employee.EmployeeId);
        }

        public int DeleteEmployeeById(int id)
        {
            int rowsAffected = 0;
            string projectEmployeeSql = @"DELETE FROM project_employee 
                                          WHERE employee_id IN (
                                            SELECT employee_id FROM employee 
                                            WHERE employee_id = @employee_id
                                          )";

            string employeeSql = @"DELETE FROM employee 
                           WHERE employee_id = @employee_id;";

            try
            {
                using (SqlConnection conn = new SqlConnection(connectionString))
                {
                    conn.Open();

                    SqlCommand cmd = new SqlCommand(projectEmployeeSql, conn);
                    cmd.Parameters.AddWithValue("@employee_id", id);

                    cmd.ExecuteNonQuery();

                    cmd = new SqlCommand(employeeSql, conn);
                    cmd.Parameters.AddWithValue("@employee_id", id);

                    rowsAffected = cmd.ExecuteNonQuery();
                }
            }
            catch (SqlException ex)
            {
                throw new DaoException("Error deleting employee", ex);
            }
            return rowsAffected;
        }

        public int DeleteEmployeesByDepartmentId(int departmentId)
        {
            int rowsAffected = 0;
            string projectEmployeeSql = @"DELETE FROM project_employee 
                                          WHERE employee_id IN (
                                            SELECT employee_id FROM employee 
                                            WHERE department_id = @department_id
                                          )";

            string employeeSql = "DELETE FROM employee WHERE department_id = @department_id";

            try
            {
                using (SqlConnection conn = new SqlConnection(connectionString))
                {
                    conn.Open();

                    SqlCommand cmd = new SqlCommand(projectEmployeeSql, conn);
                    cmd.Parameters.AddWithValue("@department_id", ((object)departmentId ?? DBNull.Value));

                    cmd.ExecuteNonQuery();

                    cmd = new SqlCommand(employeeSql, conn);
                    cmd.Parameters.AddWithValue("@department_id", departmentId);

                    rowsAffected = cmd.ExecuteNonQuery();
                }
            }
            catch (SqlException ex)
            {
                throw new DaoException("Error deleting employees by department", ex);
            }
            return rowsAffected;
        }

        private Employee MapRowToEmployee(SqlDataReader reader)
        {
            Employee employee = new Employee();
            employee.EmployeeId = Convert.ToInt32(reader["employee_id"]);
            employee.DepartmentId = Convert.ToInt32(reader["department_id"]);
            employee.FirstName = Convert.ToString(reader["first_name"]);
            employee.LastName = Convert.ToString(reader["last_name"]);
            employee.BirthDate = Convert.ToDateTime(reader["birth_date"]);
            employee.HireDate = Convert.ToDateTime(reader["hire_date"]);

            return employee;
        }


    }
}
