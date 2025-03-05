// This service will use data in an external API
import { Injectable } from '@angular/core';
import { HttpClient }  from '@angular/common/http'; 
import { HttpHeaders } from '@angular/common/http'; 
import { lastValueFrom } from 'rxjs';
import { bookClubMemberModel } from '../bookClubMemberModel';

@Injectable({
  providedIn: 'root'
})
export class BookClubService {

  private franksServerURL = "https://localhost:7074/ClubMembers";

  constructor(private franksServer : HttpClient) {}

  // Method to get all members from the external API data source
  // Do an Http GET to the server
  async getAllMembers()  : Promise<any[]> { 
    const result : any[] =  
          await lastValueFrom(this.franksServer.get<any[]>(this.franksServerURL))
    return result;      
  }   

  // Method to send an updated object to the server
  // We need to do an HTTP PUT request to the server
  async ryansUpdateRequest(updateMember : bookClubMemberModel) {

    // Define an HTTP Header to tell server what kind of data we are sending
    const headers = new HttpHeaders ({ 
      'Content-Type' : 'application/json' 
       }); 

    // Make the HTTP PUT call to server with the updated data and headers
    // The path for a PUT to the server wants the id added to root path
    lastValueFrom(this.franksServer.put(this.franksServerURL+"/"+updateMember.id   // Server URL to use
                                       , updateMember                              // Object with the updated data
                                       ,{headers}));                               // Headers to tell server the data type we are sending
                                      }
    // Since no further process is done after the API call await was not required when we made the call

  // Method to delete a member from the server
  // The path for a DELETE to server wants the id added to root path
  async deleteAMember(member2Delete : bookClubMemberModel) {
     await lastValueFrom(this.franksServer.delete(this.franksServerURL+"/"+ member2Delete.id))
     // In case we add processing after teh API DELETE call, we added await to the API call
     //    so we don't continue until th server returns from the process
  }

  // Method to add a member to serve data source
  // The root path to the server does NOT require any additional information
  async addAMember(newMember : bookClubMemberModel) {
 
    console.log(newMember)
    //newMember.id = "10"
    // Define an HTTP Header to tell server what kind of data we are sending
    const headers = new HttpHeaders ({ 
      'Content-Type' : 'application/json' 
      }); 

    // Make the HTTP POST call to server with the new data and headers
    // The path for a POST to the server wants just the root path
    await lastValueFrom(this.franksServer.post(this.franksServerURL    // Server URL to use
                                             , newMember                // Object with the new data
                                             ,{headers}));              // Headers to tell server the data type we are sending
        }
    
  }

