package com.frank.hotels;

import com.frank.hotels.models.Hotel;
import com.frank.hotels.models.Reservation;
import com.frank.hotels.services.ConsoleService;
import com.frank.hotels.services.HotelService;

public class App {

    private static final String API_BASE_URL = "http://localhost:8080/";

    public static void main(String[] args) {
        int menuSelection = 999;
        int hotelId = -1;

        ConsoleService consoleService = new ConsoleService();
        HotelService hotelService = new HotelService(API_BASE_URL);

        while (menuSelection != 0) {
            menuSelection = consoleService.printMainMenu();
            switch (menuSelection) {
                case 1:
                    {
                        // List all hotels
                        Hotel[] hotels = hotelService.listHotels();
                        if (hotels != null) {
                            consoleService.printHotels(hotels);
                        }       break;
                    }
                case 2:
                    {
                        // List Reservations for hotel
                        Hotel[] hotels = hotelService.listHotels();
                        if (hotels != null) {
                            hotelId = consoleService.promptForHotel(hotels, "List Reservations");
                            if (hotelId != 0) {
                                Reservation[] reservations = hotelService.listReservationsByHotel(hotelId);
                                if (reservations != null) {
                                    consoleService.printReservations(reservations, hotelId);
                                }
                            }
                        }       break;
                    }
                case 3:
                    // Create new reservation for a given hotel
                    String newReservationString = consoleService.promptForReservationData();
                    Reservation reservation = hotelService.addReservation(newReservationString);
                    // if unsuccessful
                    if (reservation == null) {
                        System.out.println(
                                "Invalid Reservation. Please enter the Hotel Id, Full Name, Checkin Date, Checkout Date and Guests");
                    }   break;
                case 4:
                    {
                        // Update an existing reservation
                        Reservation[] reservations = hotelService.listReservations();
                        if (reservations != null) {
                            int reservationId = consoleService.promptForReservation(reservations, "Update Reservation");
                            Reservation existingReservation = hotelService.getReservation(reservationId);
                            if (existingReservation != null) {
                                String csv = "" + reservationId + ",";
                                csv += consoleService.promptForReservationData();
                                Reservation deleteReservation = hotelService.updateReservation(csv);
                                if (deleteReservation == null) {
                                    System.out.println(
                                            "Invalid Reservation. Hotel Id, Full Name, Checkin Date, Checkout Date, Guests");
                                }
                            }
                        }       break;
                    }
                case 5:
                    {
                        // Delete reservation
                        Reservation[] reservations = hotelService.listReservations();
                        if (reservations != null) {
                            int reservationId = consoleService.promptForReservation(reservations, "Delete Reservation");
                            hotelService.deleteReservation(reservationId);
                        }       break;
                    }
                case 0:
                    // Exit
                    consoleService.exit();
                    break;
                default:
                    // anything else is not valid
                    System.out.println("Invalid Selection");
                    break;
            }
            // Press any key to continue...
            if (hotelId != 0) {
                consoleService.next();
            }
            // Ensure loop continues
            menuSelection = 999;
        }
        // if the loop exits, so does the program
        consoleService.exit();
    }

}
