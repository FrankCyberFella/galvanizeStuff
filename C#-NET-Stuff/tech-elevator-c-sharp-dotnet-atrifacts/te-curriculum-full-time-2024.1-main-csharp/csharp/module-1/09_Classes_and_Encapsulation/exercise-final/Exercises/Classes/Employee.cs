using System;

namespace Exercises.Classes
{
    public class Employee
    {
        /// <summary>
        /// Employee Id
        /// </summary>
        public int EmployeeId { get; private set; }


        /// <summary>
        /// Employee Name
        /// </summary>
        public string FirstName { get; private set; }


        /// <summary>
        /// Employee last name
        /// </summary>
        public string LastName { get; set; }


        public string FullName
        {
            get { return $"{LastName}, {FirstName}"; }
        }

        /// <summary>
        /// Department
        /// </summary>
        public string Department { get; set; }


        /// <summary>
        /// Annual Employee Salary
        /// </summary>
        public double AnnualSalary { get; private set; }


        /// <summary>
        /// Creates a new employee
        /// </summary>
        /// <param name="name">Employee name</param>
        /// <param name="annualSalary"></param>
        public Employee(int employeeId, string firstName, string lastName, double annualSalary)
        {
            this.EmployeeId = employeeId;
            this.FirstName = firstName;
            this.LastName = lastName;
            this.AnnualSalary = annualSalary;
        }


        /// <summary>
        /// Provides the employee a percentage-based raise
        /// </summary>
        /// <param name="percentage">number-based percentage to raise salary by</param>
        public void RaiseSalary(double percentage)
        {
            double raiseAmount = AnnualSalary * Math.Abs(percentage/100.00);
            AnnualSalary += raiseAmount;
        }
    }
}
