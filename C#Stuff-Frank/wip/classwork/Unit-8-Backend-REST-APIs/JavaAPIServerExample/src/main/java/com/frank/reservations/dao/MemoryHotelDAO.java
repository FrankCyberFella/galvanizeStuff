package com.frank.reservations.dao;

import java.util.ArrayList;
import java.util.List;

import com.frank.reservations.models.Address;
import com.frank.reservations.models.Hotel;

/***********************************************************************************
 * a DAO (Data Access Object) is used to manipulate data in a data source
 * 
 * Usually the name of the DAO give you idea of the data source
 * 
 * Contains the methods for accessing a data source
 ***********************************************************************************/
public class MemoryHotelDAO implements HotelDAO {

    /**
     * We are using a static variable here because this DAO is used by the hotel
     * controller and the MemoryReservationDAO and they need to share the same data.
     * This is where we would normally reach for dependency injection but because we
     * haven't learned about it yet we are using this workaround.
     */
    // The data source ia a List in memory
    // static creates one instance of teh data shared by all who use the DAO
    public static List<Hotel> hotels = new ArrayList<>();

    public MemoryHotelDAO() {
        if (hotels.size() == 0) {
            setHotels();
        }
    }

    // Return a Hotel for the id or null if not in the data source
    @Override  // Ask the compiler to verify this is a correct override for the interface
    public Hotel get(int id) {
        for (Hotel hotel : hotels) {
            if (hotel.getId() == id) {
                return hotel;
            }
        }
        return null;
    }

    // Return all the objects in the data source
    @Override
    public List<Hotel> list() {
        return hotels;
    }

    // Add an object to the data source
    @Override
    public void create(Hotel hotel) {
        hotels.add(hotel);
    }

    // INitialize our data source
    private void setHotels() {
        hotels.add(new Hotel(1,
                "Aloft Cleveland",
                new Address("1111 W 10th St","","Cleveland","Ohio","44115"),
                4,
                48,274));
        hotels.add(new Hotel(2,
                "Hilton Cleveland Downtown",
                new Address("100 Lakeside Ave","","Cleveland","Ohio","44114"),
                4,
                12,287));
        hotels.add(new Hotel(3,
                "Metropolitan at the 9",
                new Address("2017 E 9th St","","Cleveland","Ohio","48226"),
                5,
                22,319));
        hotels.add(new Hotel(4,
                "The Westin Pittsburgh",
                new Address("1000 Penn Ave","","Pittsburgh","Pennsylvania","15222"),
                4,
                60,131));
        hotels.add(new Hotel(5,
                "Hilton Columbus Downtown",
                new Address("401 N High St","","Columbus","Ohio","43215"),
                4,
                34,190));
        hotels.add(new Hotel(6,
                "The Summit A Dolce Hotel",
                new Address("5345 Medpace Way","","Cincinnati","Ohio","43215"),
                4,
                43,218));
        hotels.add(new Hotel(7,
                "Greektown Detroit",
                new Address("1200 St Antoine St","","Detroit","Michigan","48226"),
                4,
                75,185));
    }
}
