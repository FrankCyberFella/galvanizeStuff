package com.frank.reservations.controllers;

import java.util.ArrayList;
import java.util.List;

import javax.validation.Valid;

import org.springframework.http.HttpStatus;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.ResponseStatus;
import org.springframework.web.bind.annotation.RestController;

import com.frank.reservations.dao.HotelDAO;
import com.frank.reservations.dao.MemoryHotelDAO;
import com.frank.reservations.dao.MemoryReservationDAO;
import com.frank.reservations.dao.ReservationDAO;
import com.frank.reservations.exception.HotelNotFoundException;
import com.frank.reservations.exception.ReservationNotFoundException;
import com.frank.reservations.models.Hotel;
import com.frank.reservations.models.Reservation;
import com.frank.reservations.utilities.LogAPIRequest;

/************************************************************************************
 * This contains the controllers for processing HTTP requests to Hotel Application
 ***********************************************************************************/

@RestController  // This tells the serve the class contains REST Http Request controller
public class HotelController {

    private final HotelDAO       hotelDAO;       // An object to access the HotelDAO (code interface name)
    private final ReservationDAO reservationDAO; // An object to access the ReservationDAO (code interface name)

    // Constructor for the class will instantiate and assign the objects to their reference
    public HotelController() {
        this.hotelDAO       = new MemoryHotelDAO();
        this.reservationDAO = new MemoryReservationDAO(hotelDAO);
    }

    /**
     * Return All Hotels
     *
     * @return a list of all hotels in the system
     * 
     * @RequestMapping identify which URL path and HTTP request the method will handle
     */
    
    @RequestMapping(path="/hotels", method = RequestMethod.GET)
    public List<Hotel> list() {
        // Uses a "utility class" to write out a message to teh server log
        //  classname.method(parameter)
        LogAPIRequest.logAPICall("GET  - /hotels"); //Optional log the request received by the server
        return hotelDAO.list(); // call teh DAO to get all the Hotels and return them
    }

    /**
     * Return hotel information
     *
     * @param id the id of the hotel
     * @return all info for a given hotel
     */
    @RequestMapping(path = "/hotels/{id}", method = RequestMethod.GET)
    public Hotel get(@PathVariable int id) {
        LogAPIRequest.logAPICall("GET  - /hotels/" + id);
        return hotelDAO.get(id);
    }

    /**
     * Returns all reservations in the system
     *
     * @return all reservations
     */
    @RequestMapping(path = "/reservations", method = RequestMethod.GET)
    public List<Reservation> listReservations() {
        LogAPIRequest.logAPICall("GET  - /reservations");
        return reservationDAO.findAll();
    }

    /**
     * Get a reservation by its id
     *
     * @param id
     * @return a single reservation
     */
    @RequestMapping(path = "reservations/{id}", method = RequestMethod.GET)
    public Reservation getReservation(@PathVariable int id) throws ReservationNotFoundException {
        LogAPIRequest.logAPICall("GET  - /reservatins/"+ id);
        return reservationDAO.get(id);
    }

    /**
     * List of reservations by hotel
     *
     * @param hotelID
     * @return all reservations for a given hotel
     * 
     * This method will handle a PathVariable passed within the URL
     * 
     * {variable-name} in the URL to indicate a variable will be passed in the path
     * 
     * The method receives the Path variable using the @PathVariable annotation in the parameter list
     * 
     * @PathVariable("name-from-path-@RequestMapping") datatype name-for-use-in-the-method
     * 
     * @PathVariable("id") int hotelID - get the path variable "id" and assign it to the method variable hotelId
     * 
     * Giving the path variable a new name for the method is optional
     */
    @RequestMapping(path = "/hotels/{id}/reservations", method = RequestMethod.GET)
    public List<Reservation> listReservationsByHotel(@PathVariable("id") int hotelID) throws HotelNotFoundException {
        LogAPIRequest.logAPICall("GET  - /hotels/" + hotelID + "/reservations");
        return reservationDAO.findByHotel(hotelID);
    }

    /**
     * Create a new reservation for a given hotel
     *
     * @param reservation
     * @param hotelID
     * 
     * Http POST sends the JSON data in the body of the request
     * 
     * @RequestBody is used in the method parameter list to tell server to create an object from the JSON it received
     * 
     * @RequestBody Reservation reservation - tells the server to create a Reservation object from the JSON in teh request
     * 
     * @Valid tells the server to apply any validation annotations in the Model POJO to teh JSON it received
     */
    // The @Valid annotation in the parameter set for the a method
    //     Tells server to valid the incoming data according to validation criteria in the class
    // If JSON fails any of the validation tests, a BadRequest HTTPStatus is returned
    @ResponseStatus(HttpStatus.CREATED)  // Set the HTTP Status for successful execution of this controller method
    @RequestMapping(path = "/hotels/{id}/reservations", method = RequestMethod.POST)
    public Reservation addReservation(@Valid @RequestBody Reservation reservation, @PathVariable("id") int hotelID)
            throws HotelNotFoundException {
        LogAPIRequest.logAPICall("POST - /hotels/" + hotelID + "/reservations");        
        return reservationDAO.create(reservation, hotelID);
    }

    /**
     * /hotels/filter?state=oh&city=cleveland
     *
     * @param state the state to filter by
     * @param city  the city to filter by
     * @return a list of hotels that match the city & state
     * 
     * This method will handle path with query parameters
     * 
     * a query parameter is at end of a path following '?'  parameterName=value
     * multiples query parameters separated by '&''
     * 
     * path for the method: /hotels/filter?state=state-value&city=Cleveland
     * 
     * you can make a query parameter optional by adding required -false to the @RequestParam
     * 
     * in this example: state is a required query parameter
     *                   city is optional
     */
    @RequestMapping(path = "/hotels/filter", method = RequestMethod.GET)
    public List<Hotel> filterByStateAndCity(@RequestParam String state, @RequestParam(required = false) String city) {
        LogAPIRequest.logAPICall("GET  - /hotels/filter?city=" + city + "?state=" + state);

        List<Hotel> filteredHotels = new ArrayList<>();  // Hold teh hotels we want
        List<Hotel> hotels = list();  // call the method in this class that gets all Hotels

        // return hotels that match state
        for (Hotel hotel : hotels) {

            // if city was passed we don't care about the state filter
            if (city != null) {
                if (hotel.getAddress().getCity().toLowerCase().equals(city.toLowerCase())) {
                    filteredHotels.add(hotel);
                }
            } else {
                if (hotel.getAddress().getState().toLowerCase().equals(state.toLowerCase())) {
                    filteredHotels.add(hotel);
                }

            }
        }
        return filteredHotels;
    }

    // Method to delete a reservation
    @RequestMapping(path="/reservations/@D" , method=RequestMethod.DELETE)
    public void deleteReservation(@PathVariable int id) throws ReservationNotFoundException {
        reservationDAO.delete(id);
    }

}
