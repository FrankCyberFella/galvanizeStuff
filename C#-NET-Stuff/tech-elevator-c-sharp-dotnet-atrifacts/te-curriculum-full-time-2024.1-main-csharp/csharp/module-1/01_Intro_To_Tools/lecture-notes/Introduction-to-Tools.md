# Introduction to Developer Tools - Lecture Notes

## **Overview of Session**
(Brief description of session and content)

You have used a computer before, from a laptop to a phone, but most of that use has been to consume content or use someone else's applications. Now we are going to create our own applications. But before we do that, we need to learn how to use our computers as tools and not just entertainment devices.

## **Session Objectives:**

(What do we want the students to be able to do upon completion)

- Students should be able to navigate files using the UI of their laptops
- Students should be able to find and open a command-line application
- Students should be able to open Visual Studio Code as a text editor
- Students should understand that there is a basic command line on their computers and how to use it
- Students should understand pathing and hierarchical, parent-child structures
- Students should remember the `cd`, `ls`, and `pwd` commands and how to use them
- Students should understand what source control is
- Students should have an understanding of what `git` is and what the workflow of it will be in the class

## **Classroom Preparation**

(What needs to be done before session starts)
Before the session starts, prepare to share the following:

1. Unix Shell Commands:
    | Command                 | Description                                 |
    |-------------------------|---------------------------------------------|
    | pwd                     | Prints out the current working directory    |
    | ls                      | List the contents of a particular directory |
    | cd `<name>`             | Change directory                            |
    | cd `<name>/<subfolder>` | Changes directory to a specific folder / subfolder |
    | cd `..`                 | Go up a directory                           |
    | cd `~`                  | Go to the user's home directory             |
    | cd `/`                  | Go to the root directory                    |
    | mkdir `<name>`          | Creates a new directory                     |
    | rmdir `<name>`          | Removes a directory                         |
    | rm `<file-name>`        | Removes a file by name                      |

2. Git Commands:
    #### See current status of working files and repository

    > Encourage them to run this after every command to see what has changed and what hasn't

    ```
    git status
    ```

    #### Pull Lecture/Solution Code
    ```
    git pull origin main
    ```

    #### Pull New Exercise Code
    ```
    git pull upstream main
    ```

    #### Push Exercise Code
    ```
    git add .
    git commit -m "My commit message"
    git push origin main
    ```

## **Agenda and Timing for Session:**
(General order of topics and timing (minutes))

* Become familiar with the laptop they will be using in class (0:15)
* Shell and File System (0:45)
* Explain purpose and benefit of using source control (0:15)
* Git Basic Command and Remote Repositories (0:45)

## **Topic List w/Notes**
(What will we teach them in order to successfully meet Module Objectives)

- ### Become familiar with the laptop they will be using in class
    - Show them how to navigate system using window manager
    - Show them where the browser is
    - Point out anything about OS that they may not be familiar with

- ### Shell and File System
    <h4>File System Definition:</h4> The file system is a method for storing and organizing files and the data that they contain. They are organized into tree structures. Drives contain folders and folders contain files.

    <h5>Analogy</h5>A hard drive is like a file cabinet. File cabinets have folders which serve as containers to various files.

    - ### File System
        - Files are the parts of the file system that contain the stuff we want. Documents, songs, spreadsheets, etc.
        - Folders hold other folders and files. *All files exist in some folder in the file system.*
        - All of these objects have metadata that describe them. Things like modified dates, names, and permissions are pieces of data that are attached to files and folders as part of the file system.

    - ### What is a Shell

        <h4>Shell Definition</h4>The <strong>shell</strong> provides a text-based interface to interacting with the file system.

        - In a shell, you write lines of code that the computer understands to get the computer to do what you want.
        - Many tasks in programming are done on the command line because it is more flexible than most GUI interfaces and can be scripted.
        - We will be using a very popular shell called `bash`.
        - On the Mac we will be using the <strong>Terminal</strong> application and the PC will use a program called <strong>Git Bash</strong>.


    - ### Navigating the File System

        - #### Changing Directories
        ```
        cd <directory name>
        ```
        - #### Print Working Directory
        ```
        pwd
        ```
        - #### List Directory Contents
        ```
        ls -al
        ```
        - #### Working with Files and Folders
            - ##### Moving Files and Folders
            ```
            mv <oldname> <newname>
            ```
            - ##### Copying Files and Folders
            ```
            cp <oldname> <newname>
            ```
            - ##### Making Folders
            ```
            mkdir <newfolder>
            ```

        > #### Instructor Led Exercise
        > (Exercise performed by the instructor with students following along their laptops)
        >
        > Walk the student through navigating the file system by opening **Explorer** or **Finder**. Show them the list view so they can see nested folders and the folder hierarchy.
        >
        > Then take them into the command line and do the same.
        >
        > ##### Take them to their home directory
        > ```
        > cd ~
        > ```
        >
        > ##### Where are you?
        > ```
        > pwd
        > ```
        >
        > ##### Show them you can see the files, folders, and their metadata
        > ```
        > ls -al
        > ```
        >
        > ##### Create a new folder
        > ```
        > mkdir testfolder
        > ```
        >
        > ##### Go into your new folder (relative)
        > ```
        > cd testfolder
        > ```
        >
        > ##### Now where are we?
        > ```
        > pwd
        > ```
        >
        > ##### How else could we have gotten there? (~)
        > ```
        > cd ~/testfolder
        > ```
        >
        > ##### How else? (absolute)
        > ```
        > cd /<absolute path to your home directory>/testfolder
        > ```
        >
        > ##### Make a new file called test.txt and save it here
        > ```
        > code test.txt
        > ```
        > Edit this file then save it
        >
        > ##### Where are we?
        > ```
        > pwd
        > ```
        >
        > ##### What's here?
        > ```
        > ls -al
        > ```
        >
        > ##### Let's rename the file (no rename command, we need to move it)
        > ```
        > mv test.txt readme.txt
        > ```
        >
        > ##### Let's make a copy of the file
        > ```
        > cp readme.txt duplicate.txt
        > ```
        >
        > ##### What's here?
        > ```
        > ls -al
        > ```
        >
        > ##### What's in duplicate?
        > ```
        > cat duplicate.txt
        > ```
        > Tell them there are many more commands to learn, if they want.
        >
        > ##### Let's delete these files and the folder
        > ```
        > cd ..
        > rm testfolder
        > ```
        >
        > ##### That didn't delete it, why?
        > ```
        > rm -r testfolder
        > ```
        >
        > ##### Points to Make
        >
        > - The words folder and directory are often used interchangeably.
        > - The place where all folders start is the root folder or root directory. It is signified by a `/`.
        > - A full path to a folder can be made by starting at the root `/` and then adding on all the folders along the path to the file, separated by `/`. EX: `/var/local/bin/code` would start at root, go into the `var` folder, then the `local` folder in that, then the `bin` folder in that and then to `code`.
        > - Your files will reside in your home folder. That will always be `~`.
        > - We will see this structure again when talking about HTML.
        > - Absolute paths specify the full path to a file or folder. There are two types of absolute paths: ones that start at the top of the file system (`/`) and ones that start at the current user's home directory (`~`).
        > - Relative paths are paths that start from where you are now, the `pwd`.
        > - In Bash on Windows, absolute paths that start with `/` must be followed by a valid drive letter. E.g. `/c/Users/Student`
        >
        >
        >    #### Caution
        >    Some students will be confused by absolute and relative pathing, so you will probably need to spend some time here explaining it a couple of times. You will also see students doing it wrong throughout the cohort.
        >
        >
        >
        >
        >    #### Walking down the path
        >    Explain to students that they should visualize walking down the path of the directories. When they start at `/` or `~`, they will then need to specify each directory that makes a path to where they want to go.
        >
        >
        >
        >
        >    #### You are here
        >    It's hard for students to know where they are in the directory structure. There's no good indicator of this in the default bash prompt. They may not get the concept that they are currently in a specific directory. Encourage them to run `pwd` whenever they can to anchor them in the file structure.
        >
        >
        > - No news is good news. If a message is shown after running a command, read it because it is probably an error. Most commands say nothing on success.
        > - Press the up arrow (&uarr;) to cycle through previous commands instead of retyping
        > - Use the `tab` key to automatically complete the path.
        > - **PC** From the PC we can also open a Git Bash window directly from the file explorer. The benefit of doing this is that it will take us directly to our intended location.

- ### Git

    ><h5>Version Control Definition</h5><span>Version Control</span> systems record changes to a file or sets of files so that previous versions can be recalled at a later point in time.

    <h5>Analogy</h5><span>Version Control</span> is analogous to a save point in a video game. We create checkpoints so that we can return to them at a later point in time if something goes wrong.

    ><h5>Git Definition</h5><span>Git</span> is a distributed version control system that keeps a copy of its changes and file sets in a repository.

    When working with Git we follow a workflow of steps.

    1. Clone - Create a copy of a remote repository on your local computer.
    2. Add - Tell Git about changes that you would like to commit.
    3. Commit - Save the change locally by creating a checkpoint or *changeset*.
    4. Push - Copy changes into a *remote* repository.

> ***IMPORTANT***
>
> If you haven't shown students how to **clone** their GitLab repository or haven't run the `setup.sh` script yet, walk them through the "Before the cohort starts / Student Instructions - Instructor led" section in the Instructor's Handbook. ***Now is the time get the first exercise onto the student laptops, and  to setup the `upstream` remote in their local repositories.***
- ### The Git Workflow
     ![The Git Workflow](img/git-workflow.png)

-   #### Git Commands

    1. **Check Status using `git status`**

    2. **Stage Changes using `git add`**



        ##### Instructor Note:
        Be sure to show `git add <file-name>`, `git add <folder-name>`, and `git add .`


        ##### CAUTION

        Running `git add` doesn't do anything except *prepare* a change to be committed. Students might think that it's putting the file into Git, but you need to really emphasize that their workflow is **both** `git add .` and then `git commit` every time they make a change.

        </div>

    3. **Committing Changes using `git commit -m "Commit Message"`**

    4. **View History using `git log`**

    5. **Git Remotes using `git remote`**
        - <div class="definition note"><h5>Local Repository Definition</h5>A <span>local repository</span> is the copy of the code on your development computer.</div>
        - <div class="definition note"><h5>Remote Repository Definition</h5>A <span>remote repository or git remote</span> is another repository that your local repository is connected to. This is often a website like GitLab that allows you to push your code to its secure backup server.</div>

    6. **Pushing and Pulling using `git push` or `git pull`**
        - **Push** - the act of copying our local committed changesets to the remote repository.
        - **Pull** - the act of downloading any new changesets from the remote repository to our local copy.


- #### Git Remote Repositories

![Git Remote Repositories](img/git-remotes.png)

> #### Instructor Led Exercise
>
> - Explain the idea of local and remote repositories and the purpose of each remote, using the following diagram:

>    ![Upstream and Remote](img/upstream-origin.png)

> - Walk the students through opening their exercises via the command line from their local exercises repository
> - Change the exercise file for today and demonstrate adding, committing and pushing the change to mimic their homework submission process using commands written on the board from Classroom Preparation step 3
>
> ##### Points to Make
>
> - Commit early and commit often. You can push as many times as you want and should do it often. Waiting until you have completed everything is a good way to forget to submit exercises. You have a free online backup of all your work, so use it.
> - `origin` is your repository that you will *push* to, `upstream` is where new exercises will go that you will *pull* from.
>
>
>
> #### The River Git
>
> You can think of upstream as you would a river. Things flow *from* upstream, so you need to pull from that to get the new stuff.
>
>
>
> - You don't need to know how every piece of this works to use it. Just remember to `add`, `commit`, and `push`.
>
>
>
> #### Caution - Workflow Mixups
>
> Students often have a hard time internalizing these commands and this workflow. Try to get them to understand that they need to `add`, then `commit` and then `push` to submit exercises. Don't worry too much about them getting this now though, they will be doing it every day from now on.
>
>

## Student Exercises (Individual)
(Exercises to be performed by individual students)

Exercises for today ask the students to write a series of commands to produce a directory structure on their machine. Scripts (`verify-part-1.sh` and `verify-part-2.sh`) allow them to validate their work. It's best if the instructor walks the students through the first two or three steps and shows them how to validate they're doing the work correctly. The scripts produce an output file to submit to show exercise completion.


## Instructor References
(Information and Links to Assist the Instructor in Doing the Lecture)


| Git Command | Description |
|-------------|-------------|
| **`git remote`** | List remote connections to another repository |
| **`git remote -v`** | List remote repositories and their URLs |
| **`git remote add <name> <url>`** | Create a new connection to a remote repository|
| **`git remote rm <name>`** | Remove a connection from a remote repository |
| **`git push <name> <branch>`** | Copies all committed changes to the remote repository |
| **`git pull <name> <branch>`** | Copies all changes in remote repository to your local |

[Git workflow diagram](http://images.osteele.com/2008/git-transport.png)



## Cheat Sheets & References
(Summaries or Links to Reference Material/Cheat Sheets)

| Reference | Description |
|-----------|-------------|
| [Git Cheat Sheet](https://drive.google.com/file/d/0Bz4DHj0l-C66UVZDZG9janljYzA/view?usp=sharing&resourcekey=0-i3Uo9hx6NozfLSQbqqwCJA) | GitHub cheat sheet. |
| [Shell Command Cheat Sheet](https://drive.google.com/open?id=0Bz4DHj0l-C66ak9ZZVc0cjNZZU0) | Shell command cheat sheet. |
