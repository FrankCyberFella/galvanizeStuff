﻿using System;

namespace VariableNaming
{
    class Program
    {
        /**
         * For the purposes of this exercise, the following naming rules are tested:
         *
         * Variable names:
         *  - must start with a lowercase character a-z.
         *  - underscore ('_') characters are not allowed.
         *  - must be at least two characters in length.
         *  - You are encouraged to create descriptive names and are required to camel case them as appropriate.
         *
         * Constant names:
         *  - must start with an uppercase character A-Z.
         *  - underscore ('_') characters are not allowed.
         *  - must be at least two characters in length.
         *  - You are encouraged to create descriptive names and are required to pascal case them as appropriate.
         *
         * Due to practical limitations, camel case and pascal case are not tested other than checking the first
         * character of the name is lower case for variables and upper case for constants. Your instructor will
         * manually review your solution checking for the correct casing.
         */

        /***********************************************************************************************************
         * NOTES TO INSTRUCTORS:
         * 1) Student solutions need to be manually checked for correct camel and pascal casing. Neither is tested
         * other than the first character checks mentioned above.
         *
         * 2) These solutions use the more common data types: int, long, and double. However, in many cases the use
         * of other smaller data types would also be acceptable. The exercise tests do not check the data type, 
         * they only verify the expected value is correct.
         * 
         * 3) The tests rely on existing the code comments. Each exercise begins with an '/* Exercise id' comment 
         * block and the tests extract the student code from one comment block to the next. If these comments are 
         * edited, removed entirely, or the solution code is not placed appropriately after the comment blocks, the 
         * tests may extract the wrong code and fail. Any test failure will display the extracted student code for 
         * review. This may reveal missing, unexpected, or merged code from multiple exercises if the comments are 
         * modified. 
         ***********************************************************************************************************/

        static void Main(string[] args)
        {
            /*
            1. 4 birds are sitting on a branch. 1 flies away. How many birds are left on
            the branch?
            */
            int birdsOnABranch = 4;
            int birdsThatFlyAway = 1;
            int birdsRemaining = birdsOnABranch - birdsThatFlyAway;

            /*
            2. There are 6 birds and 3 nests. How many more birds are there than
            nests?
            */
            int numberOfBirds = 6;
            int numberOfNests = 3;
            int numberOfExtraBirds = numberOfBirds - numberOfNests;

            /*
            3. 3 raccoons are playing in the woods. 2 go home to eat dinner. How
            many raccoons are left in the woods?
            */
            int raccoonsInTheWoods = 3;
            int raccoonsWhoGoHome = 2;
            int remainingRaccoons = raccoonsInTheWoods - raccoonsWhoGoHome;

            /*
            4. There are 5 flowers and 3 bees. How many less bees than flowers?
            */
            int numberOfFlowers = 5;
            int numberOfBees = 3;
            int shortageOfBees = numberOfFlowers - numberOfBees;

            /*
            5. 1 lonely pigeon was eating breadcrumbs. Another pigeon came to eat
            breadcrumbs, too. How many pigeons are eating breadcrumbs now?
            */
            int numberOfPigeons = 1;
            int numberOfPigeonsWhoJoin = 1;
            int totalNumberOfPigeons = numberOfPigeons + numberOfPigeonsWhoJoin;

            /*
            6. 3 owls were sitting on the fence. 2 more owls joined them. How many
            owls are on the fence now?
            */
            int owlsOnAFence = 3;
            int owlsWhoJoin = 2;
            int totalOwls = owlsOnAFence + owlsWhoJoin;

            /*
            7. 2 beavers were working on their home. 1 went for a swim. How many
            beavers are still working on their home?
            */
            int beaversWorkingOnTheirHome = 2;
            int amountOfBeaversSwimming = 1;
            int remainingBeavers = beaversWorkingOnTheirHome - amountOfBeaversSwimming;

            /*
            8. 2 toucans are sitting on a tree limb. 1 more toucan joins them. How
            many toucans in all?
            */
            int toucansOnATreeLimb = 2;
            int numberOfNewToucans = 1;
            int totalToucans = toucansOnATreeLimb + numberOfNewToucans;

            /*
            9. There are 4 squirrels in a tree with 2 nuts. How many more squirrels
            are there than nuts?
            */
            int totalSquirrels = 4;
            int totalNuts = 2;
            int differenceInSquirrels = totalSquirrels - totalNuts;

            /*
            10. Mrs. Hilt found a quarter, 1 dime, and 2 nickels. How much money did
            she find?
            */
            int quarterCount = 1;
            int dimeCount = 1;
            int nickelCount = 2;
            //use the M suffix to make sure it realizes .25 is a decimal and not a double, same goes for dime and nickels
            decimal totalCents = (quarterCount * 0.25M) + (dimeCount * .10M) + (nickelCount * .05M);

            /*
            11. Mrs. Hilt's favorite first grade classes are baking muffins. Mrs. Brier's
            class bakes 18 muffins, Mrs. MacAdams's class bakes 20 muffins, and
            Mrs. Flannery's class bakes 17 muffins. How many muffins does first
            grade bake in all?
            */
            int briersClass = 18;
            int macadamsClass = 20;
            int flanneryClass = 17;
            int totalMuffins = briersClass + macadamsClass + flanneryClass;

            /*
            12. Mrs. Hilt bought a yoyo for 24 cents and a whistle for 14 cents. How
            much did she spend in all for the two toys?
            */
            decimal yoyoCost = 0.24M;
            decimal whistleCost = .14M;
            decimal totalSpent = yoyoCost + whistleCost;

            /*
            13. Mrs. Hilt made 5 Rice Krispies Treats. She used 8 large marshmallows
            and 10 mini marshmallows. How many marshmallows did she use
            altogether?
            */
            int largeMarshmallows = 8;
            int smallMarshmallows = 10;
            int totalMallow = largeMarshmallows + smallMarshmallows;

            /*
            14. At Mrs. Hilt's house, there was 29 inches of snow, and Brecknock
            Elementary School received 17 inches of snow. How much more snow
            did Mrs. Hilt's house have?
            */
            int inchesOfSnowAtHouse = 29;
            int inchesOfSnowAtSchool = 17;
            int differenceInInches = inchesOfSnowAtHouse - inchesOfSnowAtSchool;

            /*
            15. Mrs. Hilt has $10. She spends $3 on a toy truck and $2.50 on a pencil
            case. How much money does she have left?
            */
            decimal startingMoney = 10M;
            decimal toyTruckCost = 3M;
            decimal pencilCost = 2.5M;
            decimal remainingMoney = startingMoney - (toyTruckCost + pencilCost);

            /*
            16. Josh had 16 marbles in his collection. He lost 7 marbles. How many
            marbles does he have now?
            */
            int marblesInCollection = 16;
            int lostMarbles = 7;
            int remainingMarbles = marblesInCollection - lostMarbles;

            /*
            17. Megan has 19 seashells. How many more seashells does she need to
            find to have 25 seashells in her collection?
            */
            int currentSeashellCount = 19;
            int remainingSeaShellsToFind = 25 - currentSeashellCount;

            /*
            18. Brad has 17 balloons. 8 balloons are red and the rest are green. How
            many green balloons does Brad have?
            */
            int totalBalloons = 17;
            int redBalloons = 8;
            int greenBalloons = totalBalloons - redBalloons;

            /*
            19. There are 38 books on the shelf. Marta put 10 more books on the shelf.
            How many books are on the shelf now?
            */
            int booksOnShelf = 38;
            int amountOfBooksAdded = 10;
            int newAmountBooksOnSelf = booksOnShelf + amountOfBooksAdded;

            /*
            20. A bee has 6 legs. How many legs do 8 bees have?
            */
            int legsPerBee = 6;
            int totalLegs = 8 * legsPerBee;

            /*
            21. Mrs. Hilt bought an ice cream cone for 99 cents. How much would 2 ice
            cream cones cost?
            */
            decimal singleConeCost = .99M;
            decimal totalConeCost = 2 * singleConeCost;

            /*
            22. Mrs. Hilt wants to make a border around her garden. She needs 125
            rocks to complete the border. She has 64 rocks. How many more rocks
            does she need to complete the border?
            */
            int totalRocksNeeded = 125;
            int numberOfRocksObtained = 64;
            int remainingRocksToAcquire = totalRocksNeeded - numberOfRocksObtained;

            /*
            23. Mrs. Hilt had 38 marbles. She lost 15 of them. How many marbles does
            she have left?
            */
            int initialMarbleCount = 38;
            int lostMarbleCount = 15;
            int remainingMarbleCount = initialMarbleCount - lostMarbleCount;

            /*
            24. Mrs. Hilt and her sister drove to a concert 78 miles away. They drove 32
            miles and then stopped for gas. How many miles did they have left to drive?
            */
            int distanceToConcertInMiles = 78;
            int distanceDrivenInMiles = 32;
            int remainingdistanceInMiles = distanceToConcertInMiles - distanceDrivenInMiles;

            /*
            25. Mrs. Hilt spent 1 hour and 30 minutes shoveling snow on Saturday
            morning and 45 minutes shoveling snow on Saturday afternoon. How
            much total time (in minutes) did she spend shoveling snow?
            */
            const int MinutesPerHour = 60;
            int timeSpentInMinutesMorning = (1 * MinutesPerHour) + 30;
            int timeSpentInMinutesAfternoon = 45;
            int totalMinutesSpent = timeSpentInMinutesMorning + timeSpentInMinutesAfternoon;

            /*
            26. Mrs. Hilt bought 6 hot dogs. Each hot dog cost 50 cents. How much
            money did she pay for all of the hot dogs?
            */
            decimal costPerHotDog = .50M;
            decimal totalSpentOn6Dogs = 6 * costPerHotDog;

            /*
            27. Mrs. Hilt has 50 cents. A pencil costs 7 cents. How many pencils can
            she buy with the money she has?
            */
            decimal costPerPencil = .07M;
            decimal startingAmount = 0.50M;

            // divide 50 cents by 07 cents to determine how many pencils can be purchased. Cast to an int to get the whole number
            int numberOfPencilsPurchased = (int)(startingAmount / costPerPencil);

            /*
            28. Mrs. Hilt saw 33 butterflies. Some of the butterflies were red and others
            were orange. If 20 of the butterflies were orange, how many of them
            were red?
            */
            int butterfliesSeen = 33;
            int orangeButterflies = 20;
            int redButterflies = butterfliesSeen - orangeButterflies;

            /*
            29. Kate gave the clerk $1.00. Her candy cost 54 cents. How much change
            should Kate get back?
            */
            decimal candyCost = .54M;
            decimal change = 1.00M - candyCost;

            /*
            30. Mark has 13 trees in his backyard. If he plants 12 more, how many trees
            will he have?
            */
            int treeCount = 13;
            int newTreesToPlant = 12;
            int newTreeCount = treeCount + newTreesToPlant;

            /*
            31. Joy will see her grandma in two days. How many hours until she sees
            her?
            */
            const int HoursPerDay = 24;
            int daysToSeeGrandma = 2;
            int hoursToSeeGrandma = daysToSeeGrandma * HoursPerDay;

            /*
            32. Kim has 4 cousins. She wants to give each one 5 pieces of gum. How
            much gum will she need?
            */
            int numberOfCousins = 4;
            int piecesOfGumPerCousin = 5;
            int totalPiecesOfGum = numberOfCousins * piecesOfGumPerCousin;

            /*
            33. Dan has $3.00. He bought a candy bar for $1.00. How much money is
            left?
            */
            decimal startingCash = 3.00M;
            decimal candyBarCost = 1.00M;
            decimal remainingCash = startingCash - candyBarCost;

            /*
            34. 5 boats are in the lake. Each boat has 3 people. How many people are
            on boats in the lake?
            */
            int numberOfBoats = 5;
            int peoplePerBoat = 3;
            int totalPeople = numberOfBoats * peoplePerBoat;

            /*
            35. Ellen had 380 legos, but she lost 57 of them. How many legos does she
            have now?
            */
            int startingLegos = 380;
            int lostLegos = 57;
            int remainingLegos = startingLegos - lostLegos;

            /*
            36. Arthur baked 35 muffins. How many more muffins does Arthur have to
            bake to have 83 muffins?
            */
            int bakedMuffinCount = 35;
            int muffinsToGo = 83 - bakedMuffinCount;

            /*
            37. Willy has 1400 crayons. Lucy has 290 crayons. How many more
            crayons does Willy have then Lucy?
            */
            int willysCrayons = 1400;
            int lucysCrayons = 290;
            int differenceInCrayons = willysCrayons - lucysCrayons;

            /*
            38. There are 10 stickers on a page. If you have 22 pages of stickers, how
            many stickers do you have?
            */
            int stickersPerPage = 10;
            int pagesOfStickers = 22;
            int totalStickers = stickersPerPage * pagesOfStickers;

            /*
            39. There are 100 cupcakes for 8 children to share. How much will each
            person get if they share the cupcakes equally?
            */
            int availableCupcakes = 100;
            int numberOfChildren = 8;
            double cupcakesPerChildren = (double)availableCupcakes / numberOfChildren;

            /*
            40. She made 47 gingerbread cookies which she will distribute equally in
            tiny glass jars. If each jar is to contain six cookies, how many
            cookies will not be placed in a jar?
            */
            int numberOfCookies = 47;
            int cookiesPerJar = 6;
            int leftOverCookies = numberOfCookies % cookiesPerJar;

            /*
            41. She also prepared 59 croissants which she plans to give to her 8
            neighbors. If each neighbor received an equal number of croissants,
            how many will be left with Marian?
            */
            int preparedCroissants = 59;
            int numberOfNeighbors = 8;
            int remainingCroissants = preparedCroissants % numberOfNeighbors;

            /*
            42. Marian also baked oatmeal cookies for her classmates. If she can
            place 12 cookies on a tray at a time, how many trays will she need to
            prepare 276 oatmeal cookies at a time?
            */
            int cookiesPerTray = 12;
            int totalCookies = 276;
            int totalTraysNeeded = totalCookies / cookiesPerTray;

            /*
            43. Marian’s friends were coming over that afternoon so she made 480
            bite-sized pretzels. If one serving is equal to 12 pretzels, how many
            servings of bite-sized pretzels was Marian able to prepare?
            */
            int totalPretzels = 480;
            int servingSize = 12;
            int totalServings = totalPretzels / servingSize;

            /*
            44. Lastly, she baked 53 lemon cupcakes for the children living in the city
            orphanage. If two lemon cupcakes were left at home, how many
            boxes with 3 lemon cupcakes each were given away?
            */
            int totalCupcakes = 53;
            int cupcakesLeftAtHome = 2;
            int cupcakesPerBox = 3;
            int boxesGivenAway = (totalCupcakes - cupcakesLeftAtHome) / cupcakesPerBox;

            /*
            45. Susie's mom prepared 74 carrot sticks for breakfast. If the carrots
            were served equally to 12 people, how many carrot sticks were left
            uneaten?
            */
            int preparedSticks = 74;
            int numberOfPeople = 12;
            int remainingSticks = preparedSticks % numberOfPeople;

            /*
            46. Susie and her sister gathered all 98 of their teddy bears and placed
            them on the shelves in their bedroom. If every shelf can carry a
            maximum of 7 teddy bears, how many shelves will be filled?
            */
            int totalTeddyBears = 98;
            int bearsPerShelf = 7;
            int totalShelves = totalTeddyBears / bearsPerShelf;

            /*
            47. Susie’s mother collected all family pictures and wanted to place all of
            them in an album. If an album can contain 20 pictures, how many
            albums will she need if there are 480 pictures?
            */
            int picturesPerAlbum = 20;
            int totalPictures = 480;
            int totalNumberOfAlbums = totalPictures / picturesPerAlbum;

            /*
            48. Joe, Susie’s brother, collected all 94 trading cards scattered in his
            room and placed them in boxes. If a full box can hold a maximum of 8
            cards, how many boxes were filled and how many cards are there in
            the unfilled box?
            */
            int numberOfTradingCards = 94;
            int cardsPerBox = 8;
            int totalFilledBoxes = numberOfTradingCards / cardsPerBox;
            int cardsInUnfilledBox = numberOfTradingCards % cardsPerBox;

            /*
            49. The Milky Way galaxy contains 300 billion stars. The Andromeda galaxy
            contains 1 trillion stars. How many stars do the two galaxies contain combined?
            */
            long milkyWayStars = 300_000_000_000;
            long andromedaStars = 1_000_000_000_000;
            long totalStars = milkyWayStars + andromedaStars;

            /*
            50. Cristina baked 17 croissants. If she planned to serve this equally to
            her seven guests, how many will each have?
            */
            int totalCroissants = 17;
            int numberOfGuests = 7;
            double croissantsPerGuest = (double)totalCroissants / (double)numberOfGuests;

            /*
            51. Bill and Jill are house painters. Bill can paint a standard room in 2.15 hours, while Jill averages
            1.90 hours. How long will it take the two painters working together to paint 5 standard rooms?
            Hint: Calculate the rate at which each painter can complete a room (rooms / hour), combine those rates,
            and then divide the total number of rooms to be painted by the combined rate.
            */
            double roomsPerHourBill = 1 / 2.15;
            double roomsPerHourJill = 1 / 1.90;
            double combinedRoomsPerHour = roomsPerHourBill + roomsPerHourJill;
            int numberOfRooms = 5;
            double timeToPaintFiveRooms = numberOfRooms / combinedRoomsPerHour;

            /*
            52. Create and assign variables to hold a first name, last name, and middle initial. Using concatenation,
            build an additional variable to hold the full name in the order of last name, first name, middle initial. The
            last and first names should be separated by a comma followed by a space, and the middle initial must end
            with a period. Use "Grace", "Hopper, and "B" for the first name, last name, and middle initial.
            Example: "John", "Smith, "D" —> "Smith, John D."
            */
            string firstName = "Grace";
            string lastName = "Hopper";
            string middleInitial = "B";
            string fullName = lastName + ", " + firstName + " " + middleInitial + ".";

            /*
            53. The distance between New York and Chicago is 800 miles, and the train has already travelled 537 miles.
            What percentage of the trip as a whole number (integer) has been completed?
            */
            int milesBetweenCities = 800;
            int milesAlreadyTravelled = 537;
            double percentCompleted = (double)milesAlreadyTravelled / milesBetweenCities;
            int integerPercentCompleted = (int)(percentCompleted * 100);

        }
    }
}
