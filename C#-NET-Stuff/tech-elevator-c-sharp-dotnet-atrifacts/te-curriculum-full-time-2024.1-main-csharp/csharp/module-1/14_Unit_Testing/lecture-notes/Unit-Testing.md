# Unit Testing - Lecture Notes
###### (05/24/2018)

## **Overview of Session**
Large software systems can get complex and the smallest changes can have unintended consequences. At the same time, agile software development requires rapid iteration and an ability to change.

*How do we verify that the components of code that we write are correct and that changes don't cause unintended consequences?*

## **Session Objectives**

* List the pros and cons associated with Manual versus Automated testing
* State the difference between Exploratory and Regression testing
* State the difference between Unit, Integration and Acceptance testing
* Create and run Unit tests
* Effectively choose the proper asserts from the MSTest framework
* Describe boundary cases and how to spot what the boundary cases are in a piece of code

## **Classroom Preparation**
* None

## **Agenda and Timing for Session**

* Software Development Lifecycle (0:20)
* Testing Overview (0:40)
* Break (0:10)
* Unit Testing (0:30)
* Tests With MSTest (0:30)
* Code Coverage (0:10) (Optional)

## **Topic List w/Notes** 

### Software Development Lifecycle

Typically before beginning the lecture on Testing, it's important to introduce to students the full SDLC and the various phases. Focus on the difference between Waterfall and Agile. ([Link that compares Waterfall vs Agile](https://www.guru99.com/waterfall-vs-agile.html))

### Software Testing Overview
- **Manual vs Automated Testing**    [Link to comparison](http://www.base36.com/2013/03/automated-vs-manual-testing-the-pros-and-cons-of-each/)
    - *What are the strengths of human testers?*
    - *What are the strengths of computers?*

    - **Manual Testing**
        - Creativity
        - Can evaluate subjective qualities like user friendliness and usability
    - ***Automated Testing***
        - Speed / Efficiency
        - Lower cost of execution
        - Accuracy
        - Dependability / Repeatability

- **Exploratory vs Regression**

    - **Exploratory Testing** explores the capabilities of the system looking for defects, missing features, or other opportunities for improvement. Almost always manual.

    - **Regression Testing** validates that existing features continue to operate as expected.

- **Unit, Integration, and Acceptance Testing**

    - **Unit Testing** is low level of testing performed by programmers that validates individual “*units*” of code *function as intended by the programmer*. Always automated.

    - **Integration Testing** is a broad category of tests that validate the integration between units of code or code and outside dependencies such as databases or network resources.

    - **Acceptance Testing** is testing performed from the perspective of a user of the system to verify that the features of the system satisfies user needs.

    - As you progress from Unit Testing -> Integration Testing -> Acceptance Testing:
        - longer runtime
        - more expensive to write
        - harder to troubleshoot

- **Other Types of Testing**
    - Discussion: *What things other than application features do tests validate?*
    - Performance / Scalability
    - Security
    - Usability
    - Accessibility
    - Portability / Compatibility

- **Who Does Testing?**
    - Dedicated software testers, different skill sets, QA vs. QC
    - Developers test their own code for correctness
    - Business people test code for usability and acceptance

### Introduction to Unit Testing
- **Reasons for Unit Testing**
   - Large software systems can get complex, each piece needs to be correct
   - Encourages programmers to think of error cases
   - Changes can have unintended consequences
   - Agile software development requires rapid iteration and ability to change
- **Good Unit Tests Are:**
    - Fast - elapsed time of running a unit test is ideally measurable in milliseconds
    - Reliable / Repeatable - if a test passed/failed once, it similarly passes/fails when run again, assuming no code changes
    - Independent - a test is runnable independently of other tests and tests do not have interactions with one another
    - Obvious - good tests clearly explain why they fail
- **General Structure of a Test**
    - Arrange - begin by arranging the conditions of the test, such as setting up test data
    - Act - perform the action of interest, meaning call the code to test
    - Assert - validate that the expected outcome occurred by means of an assertion, meaning check the returned value is correct, or that a file exists with the correct content
- **Unit Testing Best Practices**
    - No external dependencies
    - One *logical* assertion per test, meaning each test only contains one "concept"
    - Good test code is of the same quality as production code
- **How to Unit Test**
    - Find boundary cases in the code
        - Is there an if statement?
            - Test around the condition that the if statement tests
        - Is there a loop?
            - Test arrays in the loop that are empty, only one element, lots of elements
        - Is an object passed in?
            - Pass in null, an empty object, an object missing values that the method expects

### Implementing Tests With MSTest
- Introduce unit testing framework
- How to organize tests
    - A Test class per class file
    - Possibly multiple test methods per method
- Each method is a test
- Naming conventions

### Code Coverage
[Link to description of Code Coverage](https://confluence.atlassian.com/clover/about-code-coverage-71599496.html)
