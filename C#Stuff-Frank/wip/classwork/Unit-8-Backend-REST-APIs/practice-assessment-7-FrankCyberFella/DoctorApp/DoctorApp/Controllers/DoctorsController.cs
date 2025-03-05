using DoctorApp.Models;
using Microsoft.AspNetCore.Http;
using Microsoft.AspNetCore.Mvc;
using Microsoft.EntityFrameworkCore;

namespace DoctorApp.Controllers
{
    [Route("api/[controller]")]
    [ApiController]
    public class DoctorsController : ControllerBase
    {

        // Define a reference to the DBContext so we can use it
        private readonly DoctorDbContext _dbContext;

        // Have the DBContext Dependedncy Injected into the constructor
        public DoctorsController(DoctorDbContext dbContext)
        {
            // Assign the DBContext object to the reference
            _dbContext = dbContext;
        }

        [HttpGet("/Doctors")]  // This controller will handle the URL path "/Doctors"
        [ProducesResponseType(StatusCodes.Status200OK)]
        // async indicates the method will do async calls or use things that do
        // Task<> is used when a async method needs to return data
        // ? after the data type indicates it's an optional paramter - null if not present
        public async Task<ActionResult<List<Doctor>>> DrRyan(String? DocType)  // method will return a List of Doctor objects
        {
            // Call Entity Framework to get all the Doctors in the table and return them as a List
            //      and store so we can search them
            List<Doctor> theDoctors = await _dbContext.Doctors.ToListAsync();

            if (DocType == null)
            {
                return theDoctors;
            }

            // Search all the doctors for those that match type requested
            List<Doctor> matches = theDoctors.FindAll(aDoctor => aDoctor.Type == DocType);
            return Ok(matches);
            // return Ok(theDoctors.FindAll(aDoctor => aDoctor.Type == DocType);  // alternate solutions
        }

        [HttpGet("/Doctors/{id}")]
        public async Task<ActionResult<Doctor>> GetDoctorById(int id) // paramter name must match the UL parameter name
        {
            var queryDoctor = await _dbContext.Doctors.FindAsync(id);

            // Conditional operator (?) is alterbatice to if-else
            //               condition ? result-if-true  : result-if-false

            return queryDoctor != null ? Ok(queryDoctor) : NotFound();
        }

        [HttpPost("/Doctors")]
        [ProducesResponseType(StatusCodes.Status201Created)]
        // [FromBody] tells server to take the JSON from the body of response and instantiate a Doctor object
        public async Task<ActionResult<Doctor>> CreateDoctor([FromBody] Doctor newDoctor)
        {
            _dbContext.Add(newDoctor);          // Add to the memory copy of the data
            await _dbContext.SaveChangesAsync();
            return newDoctor;
        }

        [HttpPut("/Doctors/{id}")]
        // [FromBody] tells server to take the JSON from the body of response and instantiate a Doctor object
        public async Task<ActionResult<Doctor>> UpdateDoctor([FromBody] Doctor newDoctor, int id)
        {
            newDoctor.Id = id;                  // Assign the id to update to doctor data passed
            _dbContext.Update(newDoctor);       // Add to the memory copy of the data
            await _dbContext.SaveChangesAsync();// Save the updated memory copy of data to actual data source
            return Ok(newDoctor);
        }
        [HttpDelete("/Doctors/{id}")]
        public async Task<ActionResult<int>> DeleteDoctor(int id)
        {
            // See if the doctor is in the data source
            Doctor byeDoc = await _dbContext.Doctors.FindAsync(id);

            if(byeDoc != null)
            {
                _dbContext.Remove(byeDoc);
                await _dbContext.SaveChangesAsync();
                return Ok(id);
            }
            return NoContent();
        }
    }
}
