package co.grandcircus.frankapiserverwithsql;

import java.util.List;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RestController;

@RestController // Tells Spring Boot this class contains REST API controllers
public class GamblerApiController {

    // @Autowired tell Spring Boot to instantiate an instance of the class/interface
    // and Dependency Inject it into the following reference
    // (you don't have have constructor instantiate the object and assign a reference)
    // (you couldn't instantiate a GamblerRepository if you wanted to since its an interface)
    @Autowired  // Connect to a Repository that is Dependency Injected
    private GamblerRepository theGamblerDataSource;

    // Simple controller method to test our setup
    @GetMapping("/")  // Handle HTTP Get request for base route path "/"
    public String testMethod() {
        return "Call to route path successful";
    }

    // Return all Gamblers in the data source
    @GetMapping("/gamblers")
    public List<Gambler> getAllTheGamblers() {
        return theGamblerDataSource.findAll();  // Use the Repository to retrieve all Gamblers
    }

}
