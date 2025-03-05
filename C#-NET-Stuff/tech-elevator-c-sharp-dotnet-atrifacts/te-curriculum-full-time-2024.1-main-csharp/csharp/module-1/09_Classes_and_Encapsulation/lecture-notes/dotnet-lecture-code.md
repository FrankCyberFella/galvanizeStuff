# Classes and Encapsulation - C# Lecture Notes

Students were introduced to classes when they learned about objects and collections. Now they'll begin writing their own classes and learn about encapsulation.

Classes consist of constructors, methods, and properties. Properties represent state, methods act as behaviors and constructors are part of the initialization process.

## Classroom preparation

* None

## Lecture code

The lecture code for today's lesson is in the `ClassesAndEncapsulationLectureFinal` solution. It models a deck of cards.

Don't push the lecture code to students but show them how to create a new project and create the classes with their input using lecture-final as guidance, or for suggestions. They have an empty solution file they can use to get started and add a project to it.

### Solution files

- Classes
  - `Card.cs`: class for representing a card in a deck of cards.
  - `Deck.cs`: class for representing a deck, uses the `Card` class.
- Stubs
  - `WoodenPencil.cs`: example class showing namespaces, properties, constructors (default and overloaded), instance methods, and static methods.
- `Program.cs`: `Main` method showing dealing through the deck of cards.

## Agenda

*For timing, you'll spend most of the class doing the group lecture exercise and talking about class design. The following topics come up organically as you go through the lecture. Feel free to stop to talk about them.*

* `WoodenPencil` walkthrough
* Class design
* Encapsulation
* Access modifiers
* Static properties

### `WoodenPencil` walkthrough

The sample class provides examples of various concepts that are covered in today's lesson. There are notes in comments for information students should know and understand.

You can walk through this class with the students to discuss different components and answer any questions:

- Namespace definition
- Class declaration
- `private` class variables
- Properties
    - *You may choose to take this moment to discuss why properties are better than public members such as data validation being able to control who can read and who can write—for example, private setter or no setter.*
    - Automatic
    - Static
    - Backing field
    - Derived
- Constructors
    - Overload
- Instance methods
    - Overload
- Static methods
- Access modifiers (seen throughout class)

### Class design

- Have the students help you identify the ideal properties and values for a card, and for a deck of cards.
- What data types can be used?

### Encapsulation

- **Encapsulation** is:
    - The packaging of data and functions into a single component.
    - Hiding the implementation details of a class to prevent other parties from setting the data to an invalid or inconsistent state and also reduce coupling.
- Should the value and suit of the card be accessible?
- Should the cards in the deck be accessible?

### Goals of encapsulation

- Encapsulation makes code extendable.
- Encapsulation makes code maintainable.
- Encapsulation promotes "loose coupling."
    - A **loosely coupled** system is one in which each of its components has, or makes use of, little or no knowledge of the definitions of other separate components.

### Access modifiers

- `private` and `public`
- Access modifiers can be applied to methods and properties.
- They control whether certain methods and properties are available for use by the users of the class, or meant only to be used internally.
- Discuss the difference between these three setters:
    ```csharp
    public int Property { get; set; }  //public set - set by inside or outside the class
    public int Property { get; private set; } //private set - set only inside the class
    public int Property { get; } //read only - can only be set in class constructor
    ```

### Static properties

- **Static** indicates it belongs to the class and not an instance.
- What might be valid static properties? Suits? Values?

### Shuffle feature

If time permits and the students are receptive to it, leave it up to them to implement a shuffle algorithm. One possible solution is provided in `Deck.cs`.
