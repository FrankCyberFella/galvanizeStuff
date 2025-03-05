using Microsoft.VisualStudio.TestTools.UnitTesting;
using System;
using System.Collections.Generic;
using USCitiesAndParks.DAO;
using USCitiesAndParks.Models;

namespace USCitiesAndParks.Tests
{
    [TestClass]
    public class ParkSqlDaoTests : BaseDaoTests
    {
        private static readonly Park PARK_1 = new Park(1, "Park 1", DateTime.Parse("1800-01-02"), 100, true);
        private static readonly Park PARK_2 = new Park(2, "Park 2", DateTime.Parse("1900-12-31"), 200, false);
        private static readonly Park PARK_3 = new Park(3, "Park 3", DateTime.Parse("2000-06-15"), 300, false);

        private ParkSqlDao dao;

        [TestInitialize]
        public override void Setup()
        {
            dao = new ParkSqlDao(ConnectionString);
            base.Setup();
        }

        [TestMethod]
        public void GetParkById_With_Valid_Id_Returns_Correct_Park()
        {
            Assert.Fail();
        }

        [TestMethod]
        public void GetParkById_With_Invalid_Id_Returns_Null_Park()
        {
            Assert.Fail();
        }

        [TestMethod]
        public void GetParksByState_With_Valid_State_Returns_Correct_Parks()
        {
            Assert.Fail();
        }

        [TestMethod]
        public void GetParksByState_With_Invalid_State_Returns_Empty_List()
        {
            Assert.Fail();
        }

        [TestMethod]
        public void CreatePark_Creates_Park()
        {
            Assert.Fail();
        }

        [TestMethod]
        public void UpdatePark_Updates_Park()
        {
            Assert.Fail();
        }

        [TestMethod]
        public void DeleteParkById_Deletes_Park()
        {
            Assert.Fail();
        }

        [TestMethod]
        public void LinkParkState_Adds_Park_To_List_Of_Parks_In_State()
        {
            Assert.Fail();
        }

        [TestMethod]
        public void UnlinkParkState_Removes_Park_From_List_Of_Parks_In_State()
        {
            Assert.Fail();
        }

        private void AssertParksMatch(Park expected, Park actual)
        {
            Assert.AreEqual(expected.ParkId, actual.ParkId);
            Assert.AreEqual(expected.ParkName, actual.ParkName);
            Assert.AreEqual(expected.DateEstablished.Date, actual.DateEstablished.Date);
            Assert.AreEqual(expected.Area, actual.Area);
            Assert.AreEqual(expected.HasCamping, actual.HasCamping);
        }
    }
}
