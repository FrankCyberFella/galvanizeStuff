# Intro to JUnit

## Instructions
1. Fork and clone `Java Intro JUnit`.
    - Check with your instructor for the link to this project.
2. Write the test class for the `LightSaber` class.
    - This has to be `LightSaberTest.java` in the `src/test/java/com/galvanize` folder.
    - Make sure to cover all the methods with your tests.
    - Make sure all your tests pass.
    - Fix any bug you find in the code while testing.
3. Once you are satisfied with your solution, check your work by making sure that:
    - You have run `./assess-project` locally and that all those tests pass.
        - For Windows PowerShell type `.\assess-project`
        - For Windows Git Bash `./assess-project`
    - You have committed and pushed all your changes to _your_ fork.
4. Submit the link to your fork below.

## Description
You are given the class `LightSaber`.

It has the following state:

- `EFFICIENCY` - The percentage of charge the lightsaber uses up per hour of use. This is static. Default value is `10.0f`.
- `charge` - The percentage of charge left on the lightsaber. Default is `100.0f`.
- `color` - The color of the lightsaber. Default is `"green"`.
- `jediSerialNumber` - A jedi serial number is used to instantiate a lightsaber &mdash; a lightsaber can only belong to one jedi

It has the following behavior:

- `setCharge()` - A method to set the charge to a particular value
- `getCharge()` - A method to return the amount of charge remaining
- `setColor()` - A method to set the lightsaber color
- `getColor()` - A method to return the current color
- `getJediSerialNumber()` - A method to return the serial number of the jedi owner
- `use(float minutes)` - A method to "use" the lightsaber for the given number of minutes. It drains the battery based on its efficiency and how long it's used
- `getMinutesRemaining()` - A method to calculate the amount of use remaining base on remaining charge and efficiency
- `recharge()` - A method to charge the lightsaber back up to 100%
