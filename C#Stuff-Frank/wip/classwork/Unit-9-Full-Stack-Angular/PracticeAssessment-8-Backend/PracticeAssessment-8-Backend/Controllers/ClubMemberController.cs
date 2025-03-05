using Microsoft.AspNetCore.Http;
using Microsoft.AspNetCore.Mvc;
using PracticeAssessment_8_Backend.Models;

namespace PracticeAssessment_8_Backend.Controllers
{
  

    [ApiController]
    public class ClubMemberController : ControllerBase
    {
        static ClubMemberDAO theClubMembers = new ClubMemberDAO();

        [HttpGet("ClubMembers")]
        public List<ClubMember> getAllClubMembers() {
            return theClubMembers.getAllClubMembers();
        }

        [HttpGet("ClubMembers/{id}")]
        public ClubMember getAllClubMemberById(int id)
        {
            return theClubMembers.getClubMemberById(id);
        }

        [HttpPost("ClubMembers")]
        [ProducesResponseType(StatusCodes.Status400BadRequest)]
        public ClubMember Create(
            [Bind("Id,Name,BooksRead, FoundingMember")] ClubMember aClubMember)
        {
            return theClubMembers.addAClubMember(aClubMember); 

        }

        [HttpDelete("Clubmembers/{id}")]
        public void deleteMember(int id)
        {
            theClubMembers.deleteAClubmember(id);
        }

        [HttpPut("ClubMembers/{id}")]
        [ProducesResponseType(StatusCodes.Status400BadRequest)]
        public ClubMember update(int id,
           [Bind("Id,Name,BooksRead, FoundingMember")] ClubMember aClubMember)
        {
            return theClubMembers.updateAMember(id, aClubMember);

        }


    }
}
