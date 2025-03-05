// interface allows us to share data description with different processes
export interface bookClubMemberModel {
    id        : string,
    name      : string,
    booksRead : number,
    foundingMember : boolean
}