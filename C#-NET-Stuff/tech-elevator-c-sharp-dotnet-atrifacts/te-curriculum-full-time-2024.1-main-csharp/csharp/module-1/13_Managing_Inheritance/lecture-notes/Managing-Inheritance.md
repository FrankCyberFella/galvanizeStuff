<link rel="stylesheet" type="text/css" media="all" href="./styles/style.css" />

# Managing Inheritance lecture

###### (05/24/2018)

## **Overview of Session**

Yesterday, we saw how inheritance and interfaces can enable polymorphism and gives a signal to other developers on how to interface and use our code.

But there are still things that other developers might do on accident with our code that we did not intend. How can we give clear signals to other developers that they should use our code in certain ways and not use it in other ways?

## **Classroom Preparation**
* You will be continuing to work on the lecture example from yesterday.

## **Session Objectives**
* Students should be able to define and use `abstract` in the context of a class and a method
* Students should be able to define and use `final` in the context of a class and a method (Java)
* Students should be able to define and use `sealed` in the context of a class and a method (C#)
* Students should understand what a design pattern is and how to research them
* Students should be able to explain the differences between public, private, and protected access
* Students should understand that many keywords in C# and Java are not for security, but for design and letting other developers know how to use your code

## **Agenda and Timing for Session**

**Note:** There are no book topics on this section

- Cats (0:20)
- Final (Java) / sealed (C#) Methods and Classes (0:30)
- Break (0:10)
- Constructor Inheritance (0:15)
- Abstract Methods and Classes (0:45)
- Protected Access Modifier (0:15)

## **Topic List w/Notes**

### Cats

<div class="note instructorDirective">

Lead the students into adding a new method to the `FarmAnimal` class. Add the method `sleep()` that sets a boolean in the animal on whether it is asleep or not. Then change the sound getter to return 'Zzzzz...' if the animal is asleep and its sound if it is awake.

> Note: For C#, the `Sound` property has been changed to have a `private` backing variable, so you can modify the `get`ter for the above change.

Then change the Old MacDonald code to put some animals to sleep. Run the Old MacDonald song and show the new functionality.

</div>

- With inheritance, it's very simple to add new functionality to existing objects
- But how do we make sure that future objects follow our convention?

### Final (sealed) Methods and Classes

<div class="note instructorDirective">

Add a new class called `Cat` and have it inherit from `FarmAnimal`. Then have `Cat` override the getter for sound to always return 'Meow' (C#: you have to add `virtual` to the `Sound` property). The Cat broke the code.

We can prevent this by adding the `final` keyword in Java, or removing `virtual` in C#, to that getter method.

We can also add the `final` (Java) or `sealed` (C#) keyword to the `Cat` class in order to prevent other cats from being written. Adding this to Cat means that nothing can inherit from it.

</div>

- Making methods final means that children can't override what the parent has defined
    - Prevents logic that is integral to the application from being overridden by a poorly behaving subclass
    - Just a design decision that should have a good reason for being
    - **Note**: It's probably best to not discuss `sealed` in the context of C# methods. However, if a student should ask, C# methods are "sealed" by default, and the developer must add the `virtual` modifier to allow the method to be overridden. The `sealed` modifier on a method is allowed in C#, but its usage is rare. It would only be used when a subclass _overrides_ a virtual method, and then wants to "seal" it.
- Making classes final / sealed means that another class can't inherit from it
    - Again, just a design decision. Should have a good reason for doing it

### Constructor Inheritance

- In `FarmAnimal`, We have a constructor, but have to implement that in the sub classes
- Constructors aren't inherited and must be redefined
- If the default constructor is not defined in the base class, you must call a valid constructor in the sub classes constructor

### Abstract Methods and Classes

<div class="definition note">An <span>abstract class</span> is a class that cannot be instantiated. It exists solely for purposes of inheritance and polymorphism. An <span>abstract method/function</span> is a method/function that does not have an implementation and must be overridden by subclasses.</div>

<div class="note instructorDirective">

We don't want an actual `FarmAnimal` object in our song. What even is a generic FarmAnimal?

To make sure no other developer thinks that they should make a `FarmAnimal` object, we can set the `FarmAnimal` class to be abstract.

</div>

- Abstract classes can not have objects created from them, but they can provide logic and structure to their subclasses.

<div class="note instructorDirective">

We also want to make sure that all of our `FarmAnimals` have an `eat()` method. But all animals eat different things and in different ways. Cows will eat things very differently than a Chicken will, but they all will `eat()`. We can *force* subclasses to have an `eat()` method defined by adding an abstract `eat()` method to `FarmAnimal`:

    public abstract String eat();

</div>

- Abstract methods are methods with no logic that *must* be implemented by concrete subclasses
- If a class has an abstract method, it *must* be an abstract class
- If a class does not override an abstract method from its parent, it must also be an abstract class

<div class="note caution">

#### Caution

At this point, some students may ask what the difference is between an Abstract Class and an Interface. *This is a good question, but hard to answer.* The main differences that you can point out to them are:

1. Abstract classes can still have implemented methods. Interfaces can have no implemented methods. _(No longer the case in Java with interface default methods in Java 8+)_
2. A class can only inherit from one other class, but can implement as many interfaces as you want it to.
3. Inheriting from a super class is kind of like making a more specialized version of that class. Inheriting just means that you can be used in the context the interface is for. There is a difference in being a kind of Book versus being Sellable or Readable.

</div>

### Protected Access Modifier (Optional)

- We looked at public and private access modifiers
    - Public members are accessible to all other objects
    - Private members are only accessible to *this* object
- There is one more semi-commonly used access modifier: protected
    - Protected means that the member is accessible by this object and any object in its inheritance tree
    - In Java, it also means any class in its package, but we discourage this use
- How to decide? Programmer and OOP author Sandi Metz suggests these classifications:
    - Public is for any set in stone methods that you want other programmers to rely on to use your object
    - Protected is for building connections between inherited classes. It lets you have methods in a parent that are accessible by its children, and vice versa, but not anyone else
    - Private is for unstable methods that may change and only have use inside the class itself
    - http://www.informit.com/articles/article.aspx?p=1834700&seqNum=5
- These will become more clear as you do OOP more, but for now, make sure all member variables are private and only make public what you want someone else to use in your object.

## Pair Exercise

Continue exercises from Inheritance Part 1.
