// typescript file goes here
console.log("Welcome to TypeScript")
/* This app will create, store and process a set of students
   Each student will have a Name, class, start date of class
   Students will be stored in an array and manipulated
   using array functions
*/
// To assure that every student has the expected attributes
// we will define an interface to be a template for a student
// You can think of an interface as a programmer defined data type
interface Student {
    studentName : string;
    class : string;
    startDate : number; // yyyymmdd
}
// Define a class that will hold and process an array of Students
class Roster {
private studentRoster: Student[]; // Array of Students
    constructor() {
        this.studentRoster = [];  // setup an initially empty array
    }

// Add a student to array
addStudent(newStudent : Student) : void {
    this.studentRoster.push(newStudent); // add the newStudent to our array
}

// Return all Students in our array
getStudents() : Student[] {
    return this.studentRoster;
}

// Find a student by Name
findStudent(aName : string) : Student | null {
    // Search the studentRoster for the Student
    // 1. Go through the entire array and look for a matches on the name - for/forEach loop
    // 2. Find the index of the element in the array that matches on the name - findIndex method

    // Using the findIndex method - return the index of the element when the condition is true from => function
    let indexOfFoundElement: number = this.studentRoster.findIndex(theElement => theElement.studentName == aName) 

    // If we found a match return the element
    if (indexOfFoundElement > -1) {  // -1 is returned if no element is found
        return this.studentRoster[indexOfFoundElement];
    }
    return null;
}
// Remove a student from the array by name
// Return  true of student was removed false if they weren't (not there)
removeStudent(theName : string) : Boolean {
 // Using the findIndex method - return the index of the element when the condition is true from => function
 let indexOfFoundElement: number = this.studentRoster.findIndex(theElement => theElement.studentName == theName) 

 // If we found a match return the element
 if (indexOfFoundElement > -1) {  // -1 is returned if no element is found
    // .splice is how we can remove an element from a JavaScript array
    // .splice(index-of-first=element-to-delete, the-number-of-consecutive-elements-to-remove)
     this.studentRoster.splice(indexOfFoundElement,1)
     return true;
 }
 return false;

}

// Sort the array by name - Use the .sort method
//
// .sort uses an arrow function to compare two elements
//
// The arrow function returns:
//
//       0 - if the elements are equal
//       1 - if the 1st element passed is greater than the 2nd passed
//      -1 - if the 1st element passed is less than the 2nd passed
//
// The elements in the original array are reordered - any saved indexes are not valid

sortByName() : void {
  this.studentRoster.sort((firstElement: Student, secondElement:Student) => {
    if(firstElement.studentName < secondElement.studentName) {
        return -1
    }
    if(firstElement.studentName > secondElement.studentName) {
        return 1
    }
    return 0
}) 

  
}

} // End of Roster class
//----------------------------------------------------------------
// Define and process out class Roster
//
let classRoster = new Roster();   // Instantiate a Roster

// Define some students to add to the Roster
//          addStudent(newStudent : Student)
//                     the properties of a Student interface
classRoster.addStudent({studentName: "Josh", class : "Programming", startDate : 20240801})
classRoster.addStudent({studentName: "Ashley", class : "Programming", startDate : 20240801})
classRoster.addStudent({studentName: "Evan", class : "Programming", startDate : 20240801})
classRoster.addStudent({studentName: "Ethan", class : "C#", startDate : 20240801})
classRoster.addStudent({studentName: "Ryan", class : "HTML", startDate : 20240801})
classRoster.addStudent({studentName: "Kendall", class : "Angular", startDate : 20240801})

// Alternative method as you would in C# or Java
let aStudent : Student = {studentName: "Frank", class : "Welding", startDate : 20250214}
classRoster.addStudent(aStudent)

// Display the elements in the classRoster

// Loop through the array of Students in the class Roster
// Get the array from the class - loop through each element displaying it
classRoster.getStudents().forEach(evan => {console.log(evan)}); 

// Find a Student
console.log(`---- Finding elements ---- `)
console.table(classRoster.findStudent("Evan"));
console.log(classRoster.findStudent("Bob"));

// Remove an element from our array
console.log(`---- Removing the Frank element ---- `)

console.log(`Was Frank removed: ${classRoster.removeStudent("Frank")}`)
console.log(`Was Frank removed: ${classRoster.removeStudent("Frank")}`)

console.log(`---- Sorting the array by name ---- `)

classRoster.sortByName();
console.table(classRoster);






