using System.Security.Cryptography.X509Certificates;

namespace PracticeAssessment_8_Backend.Models
{
    public class ClubMemberDAO
    {
        static private List<ClubMember> clubMembers = new List<ClubMember>();

        static private int nextId = 6;

        public ClubMemberDAO()
        {
            clubMembers.Add(new ClubMember(1, "Frank", 10, false));
            clubMembers.Add(new ClubMember(2, "Yoda", 99999, true));
            clubMembers.Add(new ClubMember(3, "James T. Kirk", 1, false));
            clubMembers.Add(new ClubMember(4, "Worf", 5, false));
            clubMembers.Add(new ClubMember(5, "Obi-Wan", 10000, true));
        }

        public List<ClubMember> getAllClubMembers()
        {
            return clubMembers;
        }

        public ClubMember getClubMemberById(int idDesired)
        {

            ClubMember clubMemberFound = null;

            foreach (var clubMember in clubMembers)
            {
                if (clubMember.Id == idDesired)
                {
                    clubMemberFound = clubMember;
                    break;
                }
            }

            return clubMemberFound;
        }

        public ClubMember addAClubMember(ClubMember aClubMember)
        {
            aClubMember.Id = nextId;
            clubMembers.Add(aClubMember);
            nextId++;
            return aClubMember;
        }


        public void deleteAClubmember(int id)
        {
            ClubMember memberToRemove = getClubMemberById(id);

            if (memberToRemove != null)
            {
                clubMembers.Remove(memberToRemove);
            }
        }

        public ClubMember updateAMember(int id, ClubMember updatedClubMember)
        {
            ClubMember existingMember = getClubMemberById(id);

            if (existingMember != null)
            {
                clubMembers.Remove(existingMember);
                clubMembers.Add(updatedClubMember);
                return updatedClubMember;
            }
            return null;


        }
    }
}
