import { Component }      from '@angular/core';
import { GamblerInfo }    from '../gamblerInfo';  // include the MoviesInfo interface
import { FormsModule }    from '@angular/forms';
import { GamblerService } from '../gambler.service-api';
import { CommonModule }   from '@angular/common';
import { Router }         from '@angular/router';

@Component({
  selector: 'add-gambler',
  standalone: true,
  imports: [FormsModule, CommonModule],
  templateUrl: './add-gambler.component.html',
  styleUrl: './add-gambler.component.css'
})
export class AddGamblerComponent {

// Note Dependency Injection of GamblerService and Route  
constructor(private gamblerService  : GamblerService, 
            private router          : Router) {}

// Define a place to hold a new movie when entered by the user 
// Data from the web page will be used to fill in the values a new movie (two-way bind)
//     variable : data-type - using the interface as a data-type
public newGambler : GamblerInfo =  {
                                 id             : 0,
                                 gambler_name   : "please enter name",
                                 address        : "please enter address",
                                 birth_date     : "yyyy-mm-dd",
                                 monthly_salary : 0
                                };

// This method method called when the submit is clicked in the html form
// it receives a new movie object with values entered on form from the user
// the it tells the router to go the /movies page
//funcname(parameter : data-type) - using interface as a data-type
  async addGambler(newGambler : GamblerInfo) {
    console.log(newGambler)
    // call the service to add the newMovie to the data source
    await this.gamblerService.addGambler(newGambler)

    const theData  = await this.gamblerService.getGamblerList(); // Initialize our moviesList from service

    this.router.navigate(['/gamblers']) // Tell the router to go to the /movies page
  }
  
  cancelButtonClicked() {
    this.router.navigate(['/gamblers']) // Tell the router to go to the /movies page
  }
}


