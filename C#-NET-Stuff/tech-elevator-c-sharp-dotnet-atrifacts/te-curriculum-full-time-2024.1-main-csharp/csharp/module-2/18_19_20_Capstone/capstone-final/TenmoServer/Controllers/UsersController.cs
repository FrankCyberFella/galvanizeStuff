using Microsoft.AspNetCore.Authorization;
using Microsoft.AspNetCore.Mvc;
using System.Collections.Generic;
using TenmoServer.DAO;
using TenmoServer.Exceptions;
using TenmoServer.Models;

namespace TenmoServer.Controllers
{
    [Route("[controller]")]
    [ApiController]
    [Authorize]
    public class UsersController : ControllerBase
    {
        private readonly IUserDao userDao;

        public UsersController(IUserDao userDao)
        {
            this.userDao = userDao;
        }

        [HttpGet]
        public IActionResult GetUsers()
        {
            try
            {
                IList<User> users = userDao.GetUsers();

                foreach (User u in users)
                {
                    u.PasswordHash = null;
                    u.Salt = null;
                }

                return Ok(users);
            }
            catch (DaoException)
            {
                return StatusCode(500);
            }
        }
    }
}
