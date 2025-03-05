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
    public class AccountController : ControllerBase
    {
        private readonly IAccountDao accountDao;
        private readonly ITransferDao transferDao;

        public AccountController(IAccountDao accountDao, ITransferDao transferDao)
        {
            this.accountDao = accountDao;
            this.transferDao = transferDao;
        }

        [HttpGet("balance")]
        public IActionResult GetBalance()
        {
            int? userId = GetCurrentUserId();
            if (!userId.HasValue)
            {
                return BadRequest();
            }

            try
            {
                Account account = accountDao.GetAccountByUserId(userId.Value);
                if (account == null)
                {
                    return NotFound();
                }
                return Ok(account.Balance);
            }
            catch (DaoException)
            {
                return StatusCode(500);
            }
        }

        [HttpGet("transfers")]
        public IActionResult GetTransfers()
        {
            int? userId = GetCurrentUserId();
            if (!userId.HasValue)
            {
                return BadRequest();
            }

            try
            {
                IList<Transfer> transfers = transferDao.GetTransfersByUserId(userId.Value);
                return Ok(transfers);
            }
            catch (DaoException)
            {
                return StatusCode(500);
            }
        }

        [HttpGet("pending")]
        public IActionResult GetPendingTransfers()
        {
            int? userId = GetCurrentUserId();
            if (!userId.HasValue)
            {
                return BadRequest();
            }

            try
            {
                IList<Transfer> transfers = transferDao.GetPendingTransfersByUserId(userId.Value);
                return Ok(transfers);
            }
            catch (DaoException)
            {
                return StatusCode(500);
            }
        }

        private int? GetCurrentUserId()
        {
            string userId = User.FindFirst("sub")?.Value;
            if (string.IsNullOrWhiteSpace(userId)) return null;
            int.TryParse(userId, out int userIdInt);
            return userIdInt;
        }
    }
}
