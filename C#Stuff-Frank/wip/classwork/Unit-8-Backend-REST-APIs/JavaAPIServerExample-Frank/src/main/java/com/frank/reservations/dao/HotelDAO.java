package com.frank.reservations.dao;

import java.util.List;

import com.frank.reservations.models.Hotel;

// Interface defines method that must be included by anyone implementing the interface
// If anything implements this interface you know they ust include the methods of interface

public interface HotelDAO {

    List<Hotel> list();         // Return all Hotel objects in the data source

    void create(Hotel hotel);   // Add a Hotel to the data source from Hotel object

    Hotel get(int id);          // Return the Hotel with the specified id

}