// Store the student list in memory

import { Injectable } from '@angular/core';

@Injectable({
  providedIn: 'root'
})
export class StudentListServiceService {

  theStudents : any[] = [];

  constructor() { 
    // Add some students
    this.theStudents.push({name: "Frank"})
    this.theStudents.push({name: "Spock"})
    this.theStudents.push({name: "Worf"})
  }
  
  getStudent() : any[] {
    return this.theStudents
  }
  addStudent() {

  }
}