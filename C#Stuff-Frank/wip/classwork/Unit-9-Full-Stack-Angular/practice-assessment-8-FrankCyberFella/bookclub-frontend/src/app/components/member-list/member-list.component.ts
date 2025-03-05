import { Component, OnInit }   from '@angular/core';
import { BookClubService }     from '../../services/book-club.service';
import { CommonModule }        from '@angular/common';
import { bookClubMemberModel } from '../../bookClubMemberModel';
import { FormsModule }         from '@angular/forms';

@Component({
  selector: 'app-member-list',
  imports: [CommonModule, FormsModule], // This is required because we are using *ngFor and Forms
  templateUrl: './member-list.component.html',
  styleUrl: './member-list.component.css'
})
export class MemberListComponent  implements OnInit {
  // This is will hold the data retrieved from the external api
  // Initially this is an empty array
  // It will be filled in when the component is loaded by Angular
  // any[] is used to avoid any potential conflict with API data

  public memberList : any[] = []

  public newMember : bookClubMemberModel = {
                                            id        : "",
                                            name      : "",
                                            booksRead : 0,
                                            foundingMember : false
  }


  // Have the service Dependency Injected into the component
  constructor(private theBookClubService  : BookClubService ) {}

  // When the component is loaded by Angular, call the service to get all the data
  async ngOnInit()  
  {    
    this.memberList = await this.theBookClubService.getAllMembers();  
    // At this point memberList should all the data from the API
  }

  // Method to increase the read count for a member when their button is click
  // Method is receiving a bookClubMemberModel object
  increaseReadCount(aMember : bookClubMemberModel) {
    aMember.booksRead++   // Add one to books read
    // Now that we have updated books read for the member
    // We need to send to the API to be stored with the server
    // Call the service method to do the update on server
    this.theBookClubService.ryansUpdateRequest(aMember)
  }

  // Method to delete a member when user clicks the button
  // Method is receiving a bookClubMemberModel object
  kickOutMember(aMember : bookClubMemberModel) {
    // Call the service method to delete the member on the server
   this.theBookClubService.deleteAMember(aMember)
   // We can remove the from our local copy of the data (array)
   // -or-
   window. location. reload()  // Refresh the component so gets the data changes
}
submitForm() {
 // console.log("I got to submitForm()")  // Used to testing and verification
 // console.log(this.newMember)           // Used to testing and verification
 
 // We need to call the service to send the newMember data to server
 this.theBookClubService.addAMember(this.newMember) 
 window. location. reload()  // Refresh the component so gets the data changes
}
}