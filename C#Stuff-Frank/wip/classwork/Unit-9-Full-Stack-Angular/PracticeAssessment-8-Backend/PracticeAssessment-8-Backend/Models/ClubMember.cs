namespace PracticeAssessment_8_Backend.Models
{
    public class ClubMember
    {
        public int    Id        { get; set; }
        public string Name      { get; set; }
        public int    BooksRead { get; set; }
        public bool   FoundingMember { get; set; }

        public ClubMember(int id, string name, int booksRead, bool foundingMember)
        {
            Id = id;
            Name = name;
            BooksRead = booksRead;
            FoundingMember = foundingMember;
        }
    }
}
