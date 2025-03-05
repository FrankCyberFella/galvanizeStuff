# Database setup (Microsoft SQL Server)

The [Intro to Tools: Microsoft SQL Server](https://lms.techelevator.com/content_link/gitlab.com/te-curriculum/intro-to-tools-lms/mssql/03-database-setup.md) unit in the LMS walks students through creating the databases that they're going to use for the next few units. The linked lesson walks through creating a new database and populating it with data in SQL Server Management Studio (SSMS).

If students didn't set up the databases prior to class, you can use the linked instructions to walk students through. For this lecture, you'll be using the `UnitedStates` database. A copy of the same SQL script is also provided in the student `lecture` folder as `create-UnitedStates-database.sql`.

* Ensure everyone has the databases set up and that SQL Server and SSMS are working.
* Walk through opening and navigating SSMS with the students.
* Make sure to explain that you can run the script again to reset the database.

## Notes

* Don't go through the SQL in the setup script today. If they ask, let students know you'll cover creating databases in a later unit.
* Make sure students understand that the `.sql` file isn't the database. It's a script that creates the database.
* Make sure students understand that SSMS isn't the database either. It's a UI for SQL Server application that runs in the background. Usually, it would run on a separate computer.
* The SQL Server process likely is already running by default on the student laptops. In case it's not, you need to start PowerShell as an Administrator and enter the following command:

```powershell
Set-ExecutionPolicy Bypass -Scope Process -Force;~\startsqlserver.ps1
```
