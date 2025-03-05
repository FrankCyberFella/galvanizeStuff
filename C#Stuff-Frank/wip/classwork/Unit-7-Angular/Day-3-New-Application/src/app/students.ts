/*****************************************************************************
 * This is an TypeScript interface used by any process working with
 * out student data
 * 
 * An interface is a user defined data type
 * 
 * It described the format of the student data
 * 
 * Allows the sharing of data formats between processes
 */
// The interface needs to be exported so other processes can use it
export interface studentData {
    name: string,
    city: string
}