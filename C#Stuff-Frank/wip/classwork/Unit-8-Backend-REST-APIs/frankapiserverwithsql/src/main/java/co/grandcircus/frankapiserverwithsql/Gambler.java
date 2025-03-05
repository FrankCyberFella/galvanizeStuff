package co.grandcircus.frankapiserverwithsql;
import jakarta.persistence.Entity;  // Give us access to Entity processing
import jakarta.persistence.GeneratedValue;
import jakarta.persistence.GenerationType;
import jakarta.persistence.Id;

@Entity // Tells Spring Boot this contains data you want it to help manage
public class Gambler {

    // Data members of class - using the same names as columns names for simplicity

    @Id  // Tells Spring Boot the variable that follows is the Primary Key identifier
	@GeneratedValue(strategy = GenerationType.IDENTITY) // Tell Spring Boot it is auto-generated
    private int    id;
    
    private String address;
    private String birth_date;
    private String gambler_name;
    private double monthly_salary;

    // Default constructor may be used by Spring Boot to instantiate objects
    public Gambler() {
    }
    
    // Full/All argument constructor may be used by Spring Boot to instantiate objects
    public Gambler(int id, String address, String birth_date, String gambler_name, double monthly_salary) {
        this.id = id;
        this.address = address;
        this.birth_date = birth_date;
        this.gambler_name = gambler_name;
        this.monthly_salary = monthly_salary;
    }
    // Need getters and setters because Spring Boot will use them when sending and receiving
    //      data to/from the server
    public int getId() {
        return id;
    }
    public void setId(int id) {
        this.id = id;
    }
    public String getAddress() {
        return address;
    }
    public void setAddress(String address) {
        this.address = address;
    }
    public String getBirth_date() {
        return birth_date;
    }
    public void setBirth_date(String birth_date) {
        this.birth_date = birth_date;
    }
    public String getGambler_name() {
        return gambler_name;
    }
    public void setGambler_name(String gambler_name) {
        this.gambler_name = gambler_name;
    }
    public double getMonthly_salary() {
        return monthly_salary;
    }
    public void setMonthly_salary(double monthly_salary) {
        this.monthly_salary = monthly_salary;
    }

    // We can us toString() in troubleshooting
    @Override
    public String toString() {
        return "Gambler [id=" + id + ", address=" + address + ", birth_date=" + birth_date + ", gambler_name="
                + gambler_name + ", monthly_salary=" + monthly_salary + "]";
    }

}
