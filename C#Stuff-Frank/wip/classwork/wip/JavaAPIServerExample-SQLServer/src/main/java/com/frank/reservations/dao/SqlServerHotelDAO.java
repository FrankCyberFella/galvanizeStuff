package com.frank.reservations.dao;

import com.frank.reservations.models.Hotel;
import org.springframework.jdbc.core.JdbcTemplate;
import org.springframework.jdbc.support.rowset.SqlRowSet;
import org.springframework.stereotype.Component;

import javax.sql.DataSource;
import java.util.ArrayList;
import java.util.List;

@Component
public class SqlServerHotelDAO implements HotelDAO {

    // define a reference to the JdbcTemplate object we will use to access Spring DAO Framework
    private JdbcTemplate theDataBase;

    // constructor for the class which takes the dataSource as a parameter
    // dataSource will be provided when this DAO is instantiated (from application program)
    public SqlServerHotelDAO(DataSource dataSource) {
        // Instantiate a JdbcTemplate object with the dataSource give and assign it to our reference
        this.theDataBase = new JdbcTemplate(dataSource);
    }


    @Override
    public List<Hotel> list() {
        List<Hotel> allRentals = new ArrayList<Hotel>();

        String selectAllRentals = "Select * from hotel"; // Note limit is for testing only

        SqlRowSet allRentalRows = theDataBase.queryForRowSet(selectAllRentals);

        while (allRentalRows.next()) {
            allRentals.add(MapRowToHotel(allRentalRows));
        }
        return allRentals;
    }

    @Override
    public void create(Hotel hotel) {

    }

    @Override
    public Hotel get(int id) {
        return null;
    }

    Hotel MapRowToHotel(SqlRowSet aRow) {
        Hotel aHotel = new Hotel();

        aHotel.setId(aRow.getInt("id"));
        aHotel.setName(aRow.getString("name"));
        aHotel.setAddress(aRow.getString("address"));
        aHotel.setRoomsAvailable(aRow.getInt("roomsAvailable"));
        aHotel.setCostPerNight(aRow.getDouble("costPerNight"));
        aHotel.setCoverImage(aRow.getString("coverImage"));

        return aHotel;

    }
}
