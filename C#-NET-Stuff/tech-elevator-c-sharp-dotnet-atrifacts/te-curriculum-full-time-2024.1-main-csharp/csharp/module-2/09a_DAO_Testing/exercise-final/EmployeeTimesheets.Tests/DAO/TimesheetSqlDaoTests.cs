using Microsoft.VisualStudio.TestTools.UnitTesting;
using System;
using System.Collections.Generic;
using EmployeeTimesheets.DAO;
using EmployeeTimesheets.Models;

namespace EmployeeTimesheets.Tests.DAO
{
    [TestClass]
    public class TimesheetSqlDaoTests : BaseDaoTests
    {
        private static readonly Timesheet TIMESHEET_1 = new Timesheet(1, 1, 1, DateTime.Parse("2021-01-01"), 1.0M, true, "Timesheet 1");
        private static readonly Timesheet TIMESHEET_2 = new Timesheet(2, 1, 1, DateTime.Parse("2021-01-02"), 1.5M, true, "Timesheet 2");
        private static readonly Timesheet TIMESHEET_3 = new Timesheet(3, 2, 1, DateTime.Parse("2021-01-01"), 0.25M, true, "Timesheet 3");
        private static readonly Timesheet TIMESHEET_4 = new Timesheet(4, 2, 2, DateTime.Parse("2021-02-01"), 2.0M, false, "Timesheet 4");

        private TimesheetSqlDao dao;

        private Timesheet testTimesheet;

        [TestInitialize]
        public override void Setup()
        {
            dao = new TimesheetSqlDao(ConnectionString);
            testTimesheet = new Timesheet(5, 2, 1, DateTime.Now.Date, 9.9M, true, "Test Timesheet");
            base.Setup();
        }

        [TestMethod]
        public void GetTimesheetById_With_Valid_Id_Returns_Correct_Timesheet()
        {
            Timesheet timesheet = dao.GetTimesheetById(1);
            Assert.IsNotNull(timesheet, "GetTimesheetById with valid id returned a null timesheet.");
            AssertTimesheetsMatch(TIMESHEET_1, timesheet, "GetTimesheetById with valid id returned the incorrect/incomplete timesheet.");

            timesheet = dao.GetTimesheetById(4);
            Assert.IsNotNull(timesheet, "GetTimesheetById with valid id returned a null timesheet.");
            AssertTimesheetsMatch(TIMESHEET_4, timesheet, "GetTimesheetById with valid id returned the incorrect/incomplete timesheet.");
        }

        [TestMethod]
        public void GetTimesheetById_With_Invalid_Id_Returns_Null_Timesheet()
        {
            Timesheet timesheet = dao.GetTimesheetById(0); // IDs begin with 1, cannot be 0
            Assert.IsNull(timesheet, "GetTimesheetById with invalid id returned a timesheet rather than null.");
        }

        [TestMethod]
        public void GetTimesheetsByEmployeeId_With_Valid_Employee_Id_Returns_List_Of_Timesheets_For_Employee()
        {
            List<Timesheet> timesheets = dao.GetTimesheetsByEmployeeId(1);
            Assert.IsNotNull(timesheets, "GetTimesheetsByEmployeeId with valid employee id returned a null list of timesheets.");
            Assert.AreEqual(2, timesheets.Count, "GetTimesheetsByEmployeeId with valid employee id returned the wrong number of timesheets in the list.");
            AssertTimesheetsMatch(TIMESHEET_1, timesheets[0], "GetTimesheetsByEmployeeId with valid employee id returned the incorrect/incomplete timesheet.");
            AssertTimesheetsMatch(TIMESHEET_2, timesheets[1], "GetTimesheetsByEmployeeId with valid employee id returned the incorrect/incomplete timesheet.");

            timesheets = dao.GetTimesheetsByEmployeeId(2);
            Assert.IsNotNull(timesheets, "GetTimesheetsByEmployeeId with valid employee id returned a null list of timesheets.");
            Assert.AreEqual(2, timesheets.Count, "GetTimesheetsByEmployeeId with valid employee id returned the wrong number of timesheets in the list.");
            AssertTimesheetsMatch(TIMESHEET_3, timesheets[0], "GetTimesheetsByEmployeeId with valid employee id returned the incorrect/incomplete timesheet.");
            AssertTimesheetsMatch(TIMESHEET_4, timesheets[1], "GetTimesheetsByEmployeeId with valid employee id returned the incorrect/incomplete timesheet.");
        }

        [TestMethod]
        public void GetTimesheetsByEmployeeId_With_Invalid_Employee_Id_Returns_Empty_List_Of_Timesheets()
        {
            List<Timesheet> timesheets = dao.GetTimesheetsByEmployeeId(99);
            Assert.IsNotNull(timesheets, "GetTimesheetsByEmployeeId with invalid employee id returned a null list of timesheets.");
            Assert.AreEqual(0, timesheets.Count, "GetTimesheetsByEmployeeId with invalid employee id returned the wrong number of timesheets in the list.");
        }

        [TestMethod]
        public void GetTimesheetsByProjectId_With_Valid_Project_Id_Returns_List_Of_Timesheets_For_Project()
        {
            List<Timesheet> timesheets = dao.GetTimesheetsByProjectId(1);
            Assert.IsNotNull(timesheets, "GetTimesheetsByProjectId with valid project id returned a null list of timesheets.");
            Assert.AreEqual(3, timesheets.Count, "GetTimesheetsByProjectId with valid project id returned the wrong number of timesheets in the list.");
            AssertTimesheetsMatch(TIMESHEET_1, timesheets[0], "GetTimesheetsByProjectId with valid project id returned the incorrect/incomplete timesheet.");
            AssertTimesheetsMatch(TIMESHEET_2, timesheets[1], "GetTimesheetsByProjectId with valid project id returned the incorrect/incomplete timesheet.");
            AssertTimesheetsMatch(TIMESHEET_3, timesheets[2], "GetTimesheetsByProjectId with valid project id returned the incorrect/incomplete timesheet.");

            timesheets = dao.GetTimesheetsByProjectId(2);
            Assert.IsNotNull(timesheets, "GetTimesheetsByProjectId with valid project id returned a null list of timesheets.");
            Assert.AreEqual(1, timesheets.Count, "GetTimesheetsByProjectId with valid project id returned the wrong number of timesheets in the list.");
            AssertTimesheetsMatch(TIMESHEET_4, timesheets[0], "GetTimesheetsByProjectId with valid project id returned the incorrect/incomplete timesheet.");
        }

        [TestMethod]
        public void GetTimesheetsByProjectId_With_Invalid_Project_Id_Returns_Empty_List_Of_Timesheets()
        {
            List<Timesheet> timesheets = dao.GetTimesheetsByProjectId(99);
            Assert.IsNotNull(timesheets, "GetTimesheetsByProjectId with invalid project id returned a null list of timesheets.");
            Assert.AreEqual(0, timesheets.Count, "GetTimesheetsByProjectId with invalid project id returned the wrong number of timesheets in the list.");
        }

        [TestMethod]
        public void CreateTimesheet_Creates_Timesheet()
        {
            Timesheet createdTimesheet = dao.CreateTimesheet(testTimesheet);
            Assert.IsNotNull(createdTimesheet, "CreateTimesheet returned a null timesheet.");
            AssertTimesheetsMatch(createdTimesheet, testTimesheet, "CreateTimesheet returned an incorret/incomplete timesheet.");

            // verify value was saved to database, retrieve it and compare values
            Timesheet retrievedTimesheet = dao.GetTimesheetById(createdTimesheet.TimesheetId);
            Assert.IsNotNull(retrievedTimesheet, "CreateTimesheet does not appear to have correctly persisted the newly created timesheet. It could not be found by id.");
            AssertTimesheetsMatch(createdTimesheet, retrievedTimesheet, "CreateTimesheet does not appear to have fully persisted the newly created timesheet. The retrieved timesheet is incorrect/incomplete.");
        }

        [TestMethod]
        public void UpdateTimesheet_Updates_Timesheet()
        {
            Timesheet timesheet = dao.GetTimesheetById(1);
            timesheet.EmployeeId = 2;
            timesheet.ProjectId = 2;
            timesheet.DateWorked = DateTime.Now.Date;
            timesheet.HoursWorked = 9.9M;
            timesheet.IsBillable = false;
            timesheet.Description = "Test";

            Timesheet updatedTimesheet = dao.UpdateTimesheet(timesheet);
            Assert.IsNotNull(updatedTimesheet, "UpdateTimesheet returned a null timesheet.");
            AssertTimesheetsMatch(timesheet, updatedTimesheet, "UpdateTimesheet returned an incorrect/incomplete timesheet.");

            // verify value was saved to database, retrieve it and compare values
            Timesheet retrievedTimesheet = dao.GetTimesheetById(1);
            Assert.IsNotNull(retrievedTimesheet, "UpdateTimesheet does not appear to have persisted the updated timesheet. It could not be found by id.");
            AssertTimesheetsMatch(updatedTimesheet, retrievedTimesheet, "UpdateTimesheet does not appear to have fully persisted the updated timesheet. The retrieved timesheet is incorrect/incomplete.");
        }

        [TestMethod]
        public void DeleteTimesheetById_Deletes_Timesheet()
        {
            int recordsAffected = dao.DeleteTimesheetById(1);

            Assert.AreEqual(1, recordsAffected, "DeleteTimesheetById did not return the correct number of rows affected.");

            // Verify the timesheet has been deleted
            Timesheet timesheet = dao.GetTimesheetById(1);
            Assert.IsNull(timesheet, "DeleteTimesheetById failed to remove timesheet from database.");
        }

        [TestMethod]
        public void GetBillableHours_Returns_Correct_Total()
        {
            decimal total = dao.GetBillableHours(1, 1);
            Assert.AreEqual(2.5M, total, "GetBillableHours returned incorrect total for multiple timesheets.");

            total = dao.GetBillableHours(2, 1);
            Assert.AreEqual(.25M, total, "GetBillableHours returned incorrect total for single timesheet.");

            total = dao.GetBillableHours(2, 2);
            Assert.AreEqual(0M, total, "GetBillableHours failed to return 0 for no matching timesheets.");
        }

        // Note that the version of this method provided to students does not have the message parameter.
        private void AssertTimesheetsMatch(Timesheet expected, Timesheet actual, string message)
        {
            Assert.AreEqual(expected.TimesheetId, actual.TimesheetId, message);
            Assert.AreEqual(expected.EmployeeId, actual.EmployeeId, message);
            Assert.AreEqual(expected.ProjectId, actual.ProjectId, message);
            Assert.AreEqual(expected.DateWorked, actual.DateWorked, message);
            Assert.AreEqual(expected.HoursWorked, actual.HoursWorked, message);
            Assert.AreEqual(expected.IsBillable, actual.IsBillable, message);
            Assert.AreEqual(expected.Description, actual.Description, message);
        }
    }
}
