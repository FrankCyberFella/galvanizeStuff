﻿using CampgroundReservations.DAO;
using CampgroundReservations.Models;
using Microsoft.VisualStudio.TestTools.UnitTesting;
using System;
using System.Collections.Generic;

namespace CampgroundReservations.Tests.DAO
{
    [TestClass]
    public class ReservationSqlDaoTests : BaseDaoTests
    {
        [TestMethod]
        public void GetReservationById_Should_ReturnSpecificReservation()
        {
            // Arrange
            ReservationSqlDao dao = new ReservationSqlDao(ConnectionString);

            // Act
            Reservation reservation = dao.GetReservationById(1);

            // Assert
            Assert.AreEqual(1, reservation.ReservationId, "Incorrect reservation returned for ID 1");
        }

        [TestMethod]
        public void CreateReservation_Should_ReturnReservationWithNewId()
        {
            // Arrange
            ReservationSqlDao dao = new ReservationSqlDao(ConnectionString);
            Reservation reservation = new Reservation() { 
                SiteId = 1,
                Name = "TEST NAME",
                FromDate = DateTime.Now.AddDays(1),
                ToDate = DateTime.Now.AddDays(3),
                CreateDate = DateTime.Now
            };

            // Act
            Reservation reservationCreated = dao.CreateReservation(reservation);

            // Assert
            Assert.AreEqual(13, reservationCreated.ReservationId, "Incorrect ID of new reservation");
        }
    }
}
