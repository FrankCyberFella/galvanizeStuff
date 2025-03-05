<link rel="stylesheet" type="text/css" media="all" href="./styles/style.css" />

# Inheritance - Lecture Notes
###### (05/22/2018)

## Overview of Session

Sometimes we have objects in our code that are similar in some way, but differ in others. We might have to define books for our application but have different types of books, like novels, comic books, and self-help books. They can all fit on a bookshelf and all have page numbers, but novels don't have pictures and self-help books don't have characters.

We could make these all separate classes, but then we have a lot of code duplication and that's a bad thing. Instead of doing that, we can use inheritance to reuse variables and methods between classes.

## **Session Objectives**

* Students should be able to describe the purpose and use of inheritance
* Students should be able to, given an existing set of classes, properly identify subclasses and a superclasses
* Students should be able to properly define and use superclasses and subclasses in an inheritance hierarchy

## **Classroom Preparation**
* None

## **Agenda and Timing for Session**
* Define Inheritance (0:20)
* Specialization and is-a (0:20)
* Implementing inheritance (0:30)
* Break (0:10)
* Polymorphism with inheritance (1:00)

## **Topic List w/Notes**

<div class="note instructorDirective">

This lecture uses the Auction code included in the lecture code. You can show off the code as you talk about the concepts and help the students build a mental model of how inheritance works. You can then write a `main` method to use the Auction classes to show how they work and step through the code to show what's happening in the code.

</div>

### Define Inheritance

<div class="note instructorDirective">

**The Lecture notes have you create an auction**

- Students are provided with an initial `Auction` class that will become the base class
- Start with demonstrating the capability of the Auction class in a new class's main method
- Discuss how we could build on this and get a reserve or a buyout auction
- Show what code would be the same between all auctions

</div>

<div class="definition note">

**Inheritance** enables a class to take on the properties and methods defined in another class. A subclass will inherit visible properties and methods from the superclass while adding members of its own.

</div>
<div class="definition note">

**A subclass** is the *derived* class that inherits the data and behaviors from another class.

</div>
<div class="definition note">

**A superclass** is the *base* class or the parent whose data and behaviors are being passed down.

</div>

### **Specializations** and **is-a**
- Derived class are specializations of a base class
- A ReserveAuction or BuyoutAuction *is a* specific type of Auction
- A GraphingCalculator is a more specific type of Calculator

### **Implementing Inheritance**

<div class="note instructorDirective">

Show one of the subclasses of Auction and how little code is in it. Then explain that it inherits from the main Auction class. Then describe the mechanics of that below.

</div>

- Inheritance operator
- `base` / `super`
- Constructor inheritance (must implement)
- Property inheritance (private vs. protected)
- Method overloading

### Polymorphism in inheritance

<div class="note instructorDirective">

Show how you can use the sub Auction classes in the main program as Auction classes. Make a `List<Auction>` and loop through them to add `Bid`s and show that main program doesn't even have to know what kind of Auction class it is.

</div>

- Specialization classes can be referred to by their base class
- `Auction auction = new ReserveAuction()`
- this promotes polymorphic code

<div class="caution note">

Inheritance is:

- Single Inheritance Only
- Transitive
- Constructors are not inherited and must be invoked with `base` or `super`

</div>

## Instructor Led Exercise

Building an Auction, Buyout Auction, and Reserved Auction. This example is directly relatable to the Bank Account exercise they will work on later.
