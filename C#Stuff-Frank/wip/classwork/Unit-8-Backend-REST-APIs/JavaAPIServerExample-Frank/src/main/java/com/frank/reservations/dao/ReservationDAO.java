package com.frank.reservations.dao;

import java.util.List;

import com.frank.reservations.exception.HotelNotFoundException;
import com.frank.reservations.exception.ReservationNotFoundException;
import com.frank.reservations.models.Reservation;

public interface ReservationDAO {

    List<Reservation> findAll();

    List<Reservation> findByHotel(int hotelID) throws HotelNotFoundException;

    Reservation get(int reservationID) throws ReservationNotFoundException;

    Reservation create(Reservation reservation, int hotelID) throws HotelNotFoundException;

    Reservation update(Reservation reservation, int id) throws ReservationNotFoundException;

    void delete(int id) throws ReservationNotFoundException;

}