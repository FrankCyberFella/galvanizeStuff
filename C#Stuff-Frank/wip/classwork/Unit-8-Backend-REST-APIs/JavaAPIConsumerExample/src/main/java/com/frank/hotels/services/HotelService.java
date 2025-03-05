package com.frank.hotels.services;

import java.util.Random;

import org.springframework.http.HttpEntity;
import org.springframework.http.HttpHeaders;
import org.springframework.http.MediaType;
import org.springframework.web.client.ResourceAccessException;
import org.springframework.web.client.RestClientResponseException;
import org.springframework.web.client.RestTemplate;

import com.frank.hotels.models.Hotel;
import com.frank.hotels.models.Reservation;

/*****************************************************************************************************
 * This service will interact with the API Server passed as a parameter
 *****************************************************************************************************/

public class HotelService {

  private final String BASE_URL;  // Hold the base url for API calls

  // RestTemplate is a Spring Framework class for performing REST API class
  // Instantiate a RestTemplate object to use to interact with the REST API Server
  private final RestTemplate restTemplate = new RestTemplate();

  // We are also using the ConsoleService defined fro the App to interact with the user
  private final ConsoleService console = new ConsoleService();

  // Constructor receive the base url as a parameter and assign it to our reference
  public HotelService(String url) {
    BASE_URL = url;
  }

  /**
   * Create a new reservation in the hotel reservation system
   * @param newReservation
   * @return Reservation
   * 
   * Http POST requires headers and data to be placed in the body
   */
  public Reservation addReservation(String newReservation) {
    // Convert comma delimited string to a Reservation
    Reservation reservation = makeReservation(newReservation);
    if(reservation == null) {
      return null;
    }

    // Define the request headers for the Http POST
    // Set Content-Type to Application JSON
    // Create an Http Entity to hold the headers and data to be sent with the body of the request

    HttpHeaders headers = new HttpHeaders();
    headers.setContentType(MediaType.APPLICATION_JSON);
    // Create an HttpEntity for Reservation object and the headers
    HttpEntity<Reservation> entity = new HttpEntity<>(reservation, headers);

    try {
      // The Server path for Post:  //http:localhost:8080/hotels/{id}/reservations
      // include the Entity object in the postForObject method call
      //                                             Server-Path-URL                                            , entity, data-type-of-returned-data
      reservation = restTemplate.postForObject(BASE_URL + "hotels/" + reservation.getHotelID() + "/reservations", entity, Reservation.class);
    } catch (RestClientResponseException ex) {
      console.printError(ex.getRawStatusCode() + " : " + ex.getStatusText());
    } catch (ResourceAccessException ex) {
      console.printError(ex.getMessage());
    }
    return reservation;
  }

  /**
   * Updates an existing reservation by replacing the old one with a new reservation
   * @param CSV
   * @return
   */
  public Reservation updateReservation(String CSV) {
    Reservation reservation = makeReservation(CSV);
    if (reservation == null) {
      return null;
    }

    HttpHeaders headers = new HttpHeaders();
    headers.setContentType(MediaType.APPLICATION_JSON);
    HttpEntity<Reservation> entity = new HttpEntity<>(reservation, headers);

    try {
      // Nothing is expected to be returned from an HTTP PUT
      // the method is called "put" NOT putForObject()
      restTemplate.put(BASE_URL + "reservations/" + reservation.getId(), entity);
    } catch (RestClientResponseException ex) {
      console.printError(ex.getRawStatusCode() + " : " + ex.getStatusText());
    } catch (ResourceAccessException ex) {
      console.printError(ex.getMessage());
    }
    return reservation;
  }

  /**
   * Delete an existing reservation
   * @param id
   */
  public void deleteReservation(int id) {
    try {
      // HTTP DELETE is not expected to return anything
      // delete() NOT deleteForObject()
      restTemplate.delete(BASE_URL + "reservations/" + id);
    } catch (RestClientResponseException ex) {
      console.printError(ex.getRawStatusCode() + " : " + ex.getStatusText());
    } catch (ResourceAccessException ex) {
      console.printError(ex.getMessage());
    }
  }

  /* DON'T MODIFY ANY METHODS BELOW */

  /**
   * List all hotels in the system
   * @return
   */
  public Hotel[] listHotels() {
    Hotel[] hotels = null;
    try {
      hotels = restTemplate.getForObject(BASE_URL + "hotels", Hotel[].class);
    } catch (RestClientResponseException ex) {
      // handles exceptions thrown by rest template and contains status codes
      console.printError(ex.getRawStatusCode() + " : " + ex.getStatusText());
    } catch (ResourceAccessException ex) {
      // i/o error, ex: the server isn't running
      console.printError(ex.getMessage());
    }
    return hotels;
  }

  /**
   * Get the details for a single hotel by id
   * @param id
   * @return Hotel
   */
  public Hotel getHotel(int id) {
    Hotel hotel = null;
    try {
      hotel = restTemplate.getForObject(BASE_URL + "hotels/" +  id, Hotel.class);
    } catch (RestClientResponseException ex) {
      console.printError(ex.getRawStatusCode() + " : " + ex.getStatusText());
    } catch (ResourceAccessException ex) {
      console.printError(ex.getMessage());
    }
    return hotel;
  }

  /**
   * List all reservations in the hotel reservation system
   * @return Reservation[]
   * 
   * Two types of exceptions may occur from RestTemplate calls:
   * 
   *   RestClientResponseException - Request problem (4xx type error)
   * 
   *   ResourceAccessException - Server problem (5xx type error)
   * 
   */
  public Reservation[] listReservations() {
    Reservation[] reservations = null;
    try {
      // Call the API server using the RestTemplate object and getForObject() method
      // 
      // getForObject(URL-FOr-Server-Path, data-type-returned-data)
      //                                                           return: array of Reservation Class objects
      //                                                                , data-type-of-returned-data
      reservations = restTemplate.getForObject(BASE_URL + "reservations", Reservation[].class);
    } catch (RestClientResponseException ex) {
      // display the        HttpStatusCOde       and    the message from error
      console.printError(ex.getRawStatusCode() + " : " + ex.getStatusText());
    } catch (ResourceAccessException ex) {
      console.printError(ex.getMessage());
    }
    return reservations;
  }

  /**
   * List all reservations by hotel id.
   * @param hotelId
   * @return Reservation[]
   */
  public Reservation[] listReservationsByHotel(int hotelId) {
    Reservation[] reservations = null;
    try {
      reservations = restTemplate.getForObject(BASE_URL + "hotels/" + hotelId + "/reservations" , Reservation[].class);
    } catch (RestClientResponseException ex) {
      console.printError(ex.getRawStatusCode() + " : " + ex.getStatusText());
    } catch (ResourceAccessException ex) {
      console.printError(ex.getMessage());
    }
    return reservations;
  }

  /**
   * Find a single reservation by the reservationId
   * @param reservationId
   * @return Reservation
   */
  public Reservation getReservation(int reservationId) {
    Reservation reservation = null;
    try {
      reservation = restTemplate.getForObject(BASE_URL + "reservations/" + reservationId, Reservation.class);
    } catch (RestClientResponseException ex) {
      console.printError(ex.getRawStatusCode() + " : " + ex.getStatusText());
    } catch (ResourceAccessException ex) {
      console.printError(ex.getMessage());
    }
    return reservation;
  }

  // This method will tae a comma delimited string and create q Reservation object from it
  private Reservation makeReservation(String CSV) {
    // Break the comma delimited string into individual pieces into a Siring array
    String[] parsed = CSV.split(",");

    // invalid input (
    if (parsed.length < 5 || parsed.length > 6 ) {
      return null;
    }

    // Add method does not include an id and only has 5 strings
    if (parsed.length == 5) {
      // Create a string version of the id and place into an array to be concatenated
      String[] withId = new String[6];
      String[] idArray = new String[]{new Random().nextInt(1000) + ""};
      // place the id into the first position of the data array
      System.arraycopy(idArray, 0, withId, 0, 1);
      System.arraycopy(parsed, 0, withId, 1, 5);
      parsed = withId;
    }

    return new Reservation(
            Integer.parseInt(parsed[0].trim()),
            Integer.parseInt(parsed[1].trim()),
            parsed[2].trim(),
            parsed[3].trim(),
            parsed[4].trim(),
            Integer.parseInt(parsed[5].trim())
    );
  }



}
