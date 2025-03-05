# Data access part 1 lecture

## Problem statement

A common requirement for most of the applications that people use is that they need to maintain a "persistent state." This means that certain interactions with the application have lasting effects that can be recalled hours, days, or weeks later.

- Order history at Amazon.com
- LinkedIn profile information
- Email messages in GMail

Applications frequently need to retrieve this data to fulfill its purpose. One of the most common ways an application stores persistent data is by using a database.

## Overview of session

The students have already seen how they can interact with a database directly by typing SQL commands into a GUI client like SQL Server Management Studio. Today they'll learn how to write application code that can interact with a database to read persistent data.

In the `lecture-final` folder there are two subfolders:
* `data-access-lecture`: the main lecture code that you'll spend the majority of the time on.
* `sql-injection-demo`: a pared-down version of the full data access lecture code that you can use to demonstrate how SQL injection works. If you don't have time to cover it today, you can cover it in the next lecture or during a review session. See [instructor notes](#instructor-notes) for a separate walkthrough of this code.

The student `lecture` folder doesn't contain the `sql-injection-demo` folder, but it's a good idea to distribute it to the students after the lecture so they can explore it more later.

## Session objectives

* Understand how to create database connections
* Understand how to execute SQL statements and use parameters
* Introduce the DAO pattern

## Instructor notes

- [Lecture Code Walkthrough](./lecture-code.md)
- [SQL Injection demo walkthrough](./sql-injection-walkthrough.md)
