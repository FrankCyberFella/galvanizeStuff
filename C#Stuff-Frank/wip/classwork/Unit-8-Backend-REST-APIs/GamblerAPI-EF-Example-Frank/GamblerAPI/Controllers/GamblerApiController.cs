using GamblerAPI.Model;
using Microsoft.AspNetCore.Mvc;
using Microsoft.EntityFrameworkCore;
using System.Diagnostics;

namespace GamblerAPI.Controllers
{
    [Route("api")]  // root path of URLs to access processing in this controller
    [ApiController] // This file contains API controllers
    public class GamblerApiController : ControllerBase
    {
        private readonly GamblerDbContext _context;           // Define a reference to the context to be used by Entity Framework
                                                              //     the '_' at the start of a name is a convention to name private variables
                                                              //     the '_name' is not required by Entity Framework 

        public GamblerApiController(GamblerDbContext context) // Constructor with Dependency Injected context
        {
            _context = context;  // Assign app context reference to DI injected context object 
        }

        // Method to handle HTTP GET for URL path: "/api/Gamblers"  - root path ("api") defined above
        // async on the method inidcates this method does async calls
        // it returns a Task object containing a List of Gambler class objects
        // HTTP server processing converts ths List of Gamblers to JSON and returns it to the client
        [HttpGet("Gamblers")]
        public async Task<List<Gambler>> GetAllGamblers() // Calls to Entity Framework are asynchrous - hence async attribute 
        {
            return await _context.Gamblers.ToListAsync(); // Call entity Framework to return all data instance in data source as a List
        }

        // Method to handle HTTP GET for URL path: "/api/Gamblers/id" where :id" is the specific id of the Gambler to be returned
        //                                          root path "api" defined above
        [HttpGet("Gamblers/{id}")]
        public async Task<Model.Gambler?> GamblerDetailsAsync(int? id)
        {
            // Call Entity Framework to retrieve Gambler with id passed in URL
            // Retrieve by primary key
            var aGambler = await _context.Gamblers
                                         .FirstOrDefaultAsync(m => m.Id == id);

            return aGambler;  // Return Gambler retrieved from data source
        }

        // Method to handle HTTP POST for URL path: "/api/Gamblers/create" - root path "api" defined above
        //     Data to be added to the data source will be passed in the body of the HTTP POST request as JSON
        // [Bind(list-of-class-variables)]  Tells Entity Framework which class valiables to use
        //                                  when creating the object being passed as a parameter
        //                                  from the JSON sent to the server
        
        [HttpPost("Gamblers/create")]
        [ProducesResponseType(StatusCodes.Status400BadRequest)]
        public async Task<ActionResult<Gambler>> Create(
            [Bind("Id,Name,Address,BirthDate,Salary")] Gambler aGambler)  // Instantiate a Gambler object using JSON in request
        {
            Debug.WriteLine("Trying to create ganbler: " + aGambler);
            // You should never add bad/invalid data to a database
            // It can be very difficult to remove
            //
            // Always validate any data you receive from an external source (like JSON from a client)

            if (ModelState.IsValid)                   // If data passed passes all validity checks...
            {
                _context.Add(aGambler);               //      Call Entity Framework to add data to data source
                await _context.SaveChangesAsync();    //      Call Entity Framework to save new data to data source

                return aGambler;                      //      Return object passed to method and added to data source
            }

            return BadRequest(aGambler);  // Data passed was invalid, Bad Request (400) HTTP status code
        }

        // Method to handle HTTP PUT for URL path: "/api/Gamblers/update" - root path "api" defined above
        //     Data to be added to the data source will be passed in the body of the HTTP PUT request as JSON
        //     Instantiates a Gambler object using JSON in request
        [HttpPut("Gamblers/update")]
        public async Task<IActionResult> Update([Bind("GamblerId,Name,Address,Salary,BirthDate")] Gambler aGambler)
        {
            if (ModelState.IsValid)   // check if data passed passes all validity checks....
            {
                try                                     // Attempt to...
                {
                    _context.Update(aGambler);          //     Call Entity Framework to update data in data source
                    await _context.SaveChangesAsync();  //     Call Entity Framework to save/commit updated data in data source
                }
                catch (DbUpdateConcurrencyException)    // If unexpected data base error occurs..
                {
                    if (GamblerExists(aGambler.Id))    //      and update item exists...
                    {
                        throw;                          //          throw the exception to next process in call stack
                    }
                    else                                //      and update item does not exist...
                    {
                        return NotFound();              //          return with Not Found (404) HTPP status code
                    }
                }
                return Ok();                            // If update was successful, return OK (200) HTTP status code
            }
            return BadRequest();                        // Data wassed was invalid - return Bad Request (400) HTTP status code
        }



        // Method to handle HTTP DELETE for URL path: "/api/Gamblers/delete/id - where id is the Gambler id to be deleted
        [HttpDelete("Gamblers/delete/{id}")] // , ActionName("Delete")]
        [ProducesResponseType(StatusCodes.Status200OK)]
        public async Task<ActionResult<int>> DeleteConfirmedAsync(int id)
        {
            var Gambler = await _context.Gamblers.FindAsync(id); // Call Entity Framework to see if tsk to be delete exists in the data source

            if (Gambler != null)                                 // If Gambler does exist...
            {
                _context.Gamblers.Remove(Gambler);               //     Call Entity Framework to mark it as removed it from the data soure
                _context.SaveChanges();                          //     Save/Commit the delete in tehdata source

            }
            return Ok();
        }

        // Helper method to determine is a particular task exists in the data source
        private bool GamblerExists(int id)
        {
            // Call Entity Framework to determine is there is any gambler in data source with id passed
            //      and return what it returns (true if there is, false if there is not)
            return (_context.Gamblers?.Any(e => e.Id == id)).GetValueOrDefault();
        }
    }  // End of GamblerApiCOntroller Class
} // End of GamblerApi.Controllers namespace
