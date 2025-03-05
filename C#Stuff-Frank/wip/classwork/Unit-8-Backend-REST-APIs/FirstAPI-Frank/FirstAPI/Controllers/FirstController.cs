using FirstAPI.Model;
using Microsoft.AspNetCore.Http;
using Microsoft.AspNetCore.Mvc;
using Microsoft.Extensions.Diagnostics.HealthChecks;

namespace FirstAPI.Controllers
{
    // [Route()] specifies the main URL path this Controller will handle
    //                                                   www.somename.com
    // This controller will handle the path: /api (http://servername:nnnn/api)
    //      unless it's overriden by [HttpGet], [HttpPost], [HttpPut], [HttpDelete]
    //[Route("api")]
    [ApiController]  // identifies the class as containing ApiControllers
    public class FirstController : ControllerBase
    {
        // Any data used by multiple methods is defined outside any metho

        // Define an instance of the DAO we want to use
        GamblerDao theData = new GamblerDao();


        // Controller methods
        //  IActionResult is a proper HTTP response object
        [HttpGet("/")] // this method will handle a GET request with nothing extra in the path
        public IActionResult AnyNameYouWant()
        {
            // OK is a response that sends a status code 200
            // and the data inside the parentheses
            return Ok("Welcome to Frank's Controller");
        }

        // Method to return all the data in the data source
        [HttpGet("/gambler")]  // path to get all Gamblers - must differeent
        public IActionResult Silence()  // controller method name is irrelevant
        {
            // The controller uses the methods in a DAO to access data
            
            // Call the DAO method and return whatever it gives us
            return Ok(theData.getGamblers());
        }

        // Method to return a specific Gambler based on thier id
        // There must be a method in the DAO to do this for us
        // We need to get the Id to find from the URL as a parameter
        [HttpGet("/gambler/{id}")]  // Handle /gambler/id
        public IActionResult GetAGambler(int id) // receive id parameter from the URL
        {
            // Use the DAO method to find a specific Gambler and return the result
            return Ok(theData.getSpecificGambler(id));
        }

        // Method to find a Gambler by name or partial name
        // We will use a query parameter
        // We will use a DAO method to find the data
        [HttpGet("/findgambler")]  // /gambler/?name=
        public IActionResult getAGamblerByName(string theName)
        {
            return Ok(theData.getGamblerByName(theName));
        }
        // Add a Gambler to the data source
        // The HTTP Post will send us the new data in the body of the response s JSON
        //     the server will automatic instantiate the Gambler object from the JSON
        //
        // There must be a DAO to add the a Gambler
        [HttpPost("/gambler")]
        public IActionResult AddAGambler(Gambler newGambler)
        {
            return Ok(theData.AddAGambler(newGambler));
        }
}
}
