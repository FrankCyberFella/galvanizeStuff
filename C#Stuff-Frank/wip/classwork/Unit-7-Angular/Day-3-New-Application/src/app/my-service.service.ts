// This is where the code for processing in the service goes
// data and functions

import { Injectable } from '@angular/core';
import { studentData}  from './students'  // give me access to this interface

@Injectable({
  providedIn: 'root'
})
export class MyServiceService {

  constructor() { }
}
