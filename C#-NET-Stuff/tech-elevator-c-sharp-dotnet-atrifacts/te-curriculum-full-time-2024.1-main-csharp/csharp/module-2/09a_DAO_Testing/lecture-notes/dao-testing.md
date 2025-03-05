# DAO testing lecture notes

A good unit test isolates the code that it tests from dependencies on outside resources. This is desirable because it usually makes the test more reliable and also easier to debug when it fails.

However, interactions with outside resources are a potential source of bugs and bad assumptions, so it's a good idea to test these too. In fact, there are classes whose primary function is to interact with outside resources. Data Access Objects (DAOs) are an example of this. To validate that a DAO is functioning correctly, you need to test it against a database. This is an example of an "integration test."

## Objectives

* What's an integration test?
* DAO integration testing

## Notes and examples

### What's an integration test?

- **Integration testing** is a broad category of tests that validate the integration between units of code, or between code and outside dependencies such as databases or network resources.

- **Integration tests:**
    - Use the same tools as unit tests, like MSTest
    - Usually slower than unit tests (but often still measured in milliseconds)
    - More complex to write and debug
    - Can have dependencies on outside resources like files or a database

### DAO integration testing

- Since DAOs exist solely for the purpose of interacting with a database, they're often best tested using an integration test.

- The point of integration tests with a database is to ensure that the DAO code functions correctly:
    * Create methods are tested by inserting mock data and verifying it can be retrieved.
    * Read methods are tested by retrieving mock data that's inserted before the test runs.
    * Update methods are tested by updating mock data and verifying it's changed.
    * Delete methods are tested by deleting mock data and verifying it's no longer retrievable.

- Tests (including integration tests) must be:
    - *Repeatable:* If the test passes/fails on first execution, it must pass/fail on second execution if no code has changed.
    - *Independent:* A test must be able to run on its own, independently of other tests, *or* together with other tests and have the same result either way.
    - *Obvious:* When a test fails, it must be as obvious as possible why it failed.

## Code

The student starting code is similar to what the end of the previous unit's lecture looked like with these changes:

* Constructors added to the model classes for use by the tests.
* The CLI isn't included since the focus is the tests today.
* DAO methods that return multiple rows have had an `ORDER BY` clause added to them to facilitate testing.
* Removed DAO methods that return a random row or a count of rows.
* Added an `UnlinkParkState()` method that removes park and state from the `park_state` table.
* Some completed tests added that you'll walk through.

You can begin with showing the students the DAOs. Besides the preceding differences, they'll look familiar from the previous unit.

Next, show the students the tests for the `State` DAO. The first thing you'll want to point out to the students is that integration tests follow the same "Arrange-Act-Assert" pattern as unit tests. Walk through what each test does for those steps, pointing out that most of the "arrange" step often occurs in a separate, shared method that runs before each test.

### Mock data

You may want to ask the students where the data is coming from that the tests are expecting. It doesn't look like the data they saw previously in the `UnitedStates` database.

This is because you want to have the data in the database in a specific known state before you run your tests. This allows your tests to be **repeatable**—they return the same results every time you run them.

Show that the DAO test classes inherit from `BaseDaoTests`. That class contains the methods that run before and after the tests. Before all the tests, the code creates a temporary database and the `test-data.sql` script configures the data to be exactly as expected by the tests. After all the tests finish, the code removes the temporary database.

Don't get bogged down in the mechanics of the temporary database. The important things to understand are that the tests don't depend on or modify any existing database on the system, and that `test-data.sql` is the place to see (or modify) what data the tests have to work with.

### Transaction

After each test runs, you want to restore the testing database to its original state so every test has the same data to work with.

To do this, you use a transaction. Starting a transaction before the test runs and rolling back the transaction after it's completed ensures the changes that test makes to the database only last for the duration of that individual test. You can point out where this rolling back occurs in the `BaseDaoTests` class.

### City DAO tests

Walk through the `City` DAO tests with the students, pointing out how it tests the CRUD methods. Note that the test `City` object gets created before each test so that it resets any values that might have gotten changed within a test.

### Park DAO tests

With the time remaining, have the students help you write tests for the `Park` DAO. You can use the `lecture-final` version to help guide the students. The curriculum team provided empty methods, and you can use the other tests as examples.
