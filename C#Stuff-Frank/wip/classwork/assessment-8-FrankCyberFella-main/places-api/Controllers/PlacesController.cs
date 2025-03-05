using Microsoft.AspNetCore.Mvc;
using Microsoft.EntityFrameworkCore;
using places_api.Models;

namespace places_api.Controllers
{
    [Route("[controller]")]
    [ApiController]
    public class PlacesController : ControllerBase
    {
        PlacesContext dbContext = new PlacesContext();

        // Get a list of places
        [HttpGet()]
        public ActionResult<IEnumerable<Place>> GetAll()
        {
            return Ok(dbContext.Places.ToList());
        }

        // Get one place by ID
        [HttpGet("{id}")]
        public ActionResult<Place> GetPlace(int id)
        {
            Place? place = dbContext.Places.Find(id);

            if (place == null)
            {
                return NotFound();
            }

            return place;
        }

        // Add an place
        [HttpPost]
        public ActionResult<Place> PostPlace(Place place)
        {
            dbContext.Places.Add(place);
            dbContext.SaveChanges();

            // The first argument is the Location header
            // e.g. /api/Places/12
            return Created($"/api/Places/{place.Id}", place);
        }

        // Update an place
        [HttpPut("{id}")]
        public IActionResult UpdatePlace(int id, Place place)
        {
            if (id != place.Id)
            {
                return BadRequest();
            }
            if (!dbContext.Places.Any(e => e.Id == id))
            {
                return NotFound();
            }
            dbContext.Places.Update(place);
            dbContext.SaveChanges();
            return Ok(place);
        }

        // Delete an place
        [HttpDelete("{id}")]
        public IActionResult DeletePlace(int id)
        {
            Place? place = dbContext.Places.Find(id);
            if (place == null)
            {
                return NotFound();
            }

            dbContext.Places.Remove(place);
            dbContext.SaveChanges();

            return NoContent();
        }
    }

}
