<link rel="stylesheet" type="text/css" media="all" href="./styles/style.css" />

# Polymorphism - Lecture Notes

## **Overview of Session**

We saw yesterday that we can use different objects in the way by using inheritance. A specialized class can be used as its base class in a program because they share the same methods between each other.

But there might be a case too where we want to use very different types of objects as the same kind of thing.

For instance, Amazon.com can sell books, but they also sell candles and dog food, which are very different kinds of things with very different data. A book has a page count, candles have a scent, and dog food has a weight, but they can all be added to your cart and bought and shipped to your house.

We can treat different objects the same way using Interfaces.

## **Session Objectives**
* Students should be able to explain what polymorphism is and how it is used with inheritance and interfaces
* Students should be able to demonstrate an understanding of where inheritance can assist in writing polymorphic code
* Students should be able to state the purpose of interfaces and how they are used
* Students should be able to implement polymorphism through inheritance
* Students should be able to implement polymorphism through interfaces

## **Classroom Preparation**
* None

## **Agenda and Timing for Session**

1. Polymorphism through Inheritance (0:30)
2. Object Contexts (0:30)
3. Break (0:10)
3. Interfaces (0:20)
4. Polymorphism through Interfaces (0:40)


## **Topic List w/Notes**

<div class="caution note">Students may have read about this topic in advance, but it doesn't add value "diving straight in with definitions." Instead rely on the lecture code to motivate discussion.</div>

<div class="note instructorDirective">

The lecture code is already set up with a simple inheritance problem: We have a number of `FarmAnimal`s and we want to sing about them. Since they are all `FarmAnimal`s, it's easy to put them all in an array of `FarmAnimal`s and put them into a loop. This is a form of polymorphism.

The classes and the OldMacDonald code is already set up for this. Review inheritance by walking the students through that code.

</div>

### **Polymorphism through Inheritance**
- As we talked about yesterday, all sub classes can be treated as their parent class
- Create a List of `FarmAnimal`s and demonstrate any animal that "is-a" Farm Animal can be added to it.

<div class="definition note">In object-oriented programming, polymorphism is the idea that something can be assigned a different meaning or usage based on the context it is referred to as. Put another way, different object can be treated as the same type of thing within a program.</div>
<br/>

### **Object Contexts**
- Currently, we're singing about animals on the farm. What if we want to sing about something that isn't a Farm Animal? What if it's our tractor?
- In code we can also generalize objects and not think of *what they are* but instead look at *what they can do*.

	<div class="analogy note">I once asked a class, "stand up if you are a musician." Of all the students that stood up, when asked what made them a musician, they all had different answers. One indicated they were a vocalist, another a drummer, another a trumpeter, etc. No musician was alike. When they were asked to "play their instruments", they know exactly what to do and didn't need to be told to "sing" or "drum", or "strum a guitar." We were able to create a generalization of this group based on something they could do. <span>Interfaces</span> allow us to do this with objects. Another student then said they wanted to be a musician and the only thing needed was for them to specify an instrument.</div>

	<div class="analogy note">I also was able to create another generalization. Students that speak multiple languages. One of them also happened to be a musician, so this also helped us get into the idea of multiple interfaces.</div>

<div class="note instructorDirective">

Create a new class for a `Tractor`. Show how you don't want it to be a `FarmAnimal` but do want to sing about it.

You *could* make the `Tractor` a subclass of `FarmAnimal` and sing about them, but that doesn't feel right. Then introduce another complication: you want to be able to sell the `FarmAnimal`s. Add a `price` to `FarmAnimal` and make another loop to print out the prices of all the items.

But you don't want to sell the `Tractor`. The `Tractor` should not be sellable, but should be singable. How can we prevent the `Tractor` from being sold?

</div>

### **Interfaces**
- Define *what something can do* or *how it can be used*, but *not how it does it*.
- An **interface** is a **contract** that defines what **methods** a user of the interface can expect
    - What are the methods I can rely on being there for this object?
- Cannot be instantiated
- Multiple Interfaces are allowed for a single object
- if Class A implements interface B, then A "is-a" B
    - And so are all its children

<div class="analogy note"><strong>Other Analogies</strong>
<ul>
	<li>gas pumps (dispense gas to car, truck, motorcycle)</li>
	<li>shirts (two arms, neck, etc)</li>
	<li>HDMI (connects to laptop to monitor, projector, television)</li>
	<li>phone chargers</li>
</ul>
</div>

## **Polymorphism through Interfaces**

<div class="note instructorDirective">

Take the Old MacDonald song and convert it to use an interface called `ISingable` in C# or `Singable` in Java. Show that you can identify, from the song, which methods the interface needs. Make an interface that has those and put it on the `FarmAnimal` class and the `Tractor` class. Make sure Tractor is no longer a `FarmAnimal` and show how you can make an array of Singable objects to sing about.

Do the same with a Sellable interface. Add an `Egg` class that we don't want to sing about but do want to sell.

Now the program doesn't need to know what the concrete objects are that it's working with. It just needs to know what these objects implement the correct interface.

</div>

- We can treat objects as the interface type to use them in a similar context, even if they aren't the same type at all.
- Go back to the shopping cart example from the beginning. All of those items are very different things, but they are all purchasable and can be added to the cart because the cart is programmed to handle the interface and not the concrete objects.

<div class="note analogy">

You can use the example of a printer driver here. A Print Center application releases a PrinterDriver interface for manufacturers to use to create software for their printers to be used in the Print Center application. The printers might use very different hardware and could be laser printers or ink jet printers but can all be used by the program because they implement the same interface.

</div>