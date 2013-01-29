## Setting RStudio and Github

1. First, got to [https://github.com](https://github.com) and create an account.

2. Install a git client.  
* For Mac: [http://git-scm.com/download/mac](http://git-scm.com/download/macs)
* For Windows: [http://git-scm.com/download/win](http://git-scm.com/download/win)

3. Go to the RStudio Preferences dialog box and make sure a Git executable is set under the Git/SVN section.

	![Enable git access](Figures/rstudio-settings.png?raw=TRUE)

4. In the main RStudio window, click `Project (None)` in the to right corner, and select `Create Project...`

5. Select `Version Control`

	![New RStudio Project](Figures/rstudio-newproject1.png?raw=TRUE)

6. Select `Git`

	![New git project](Figures/rstudio-newproject2.png?raw=TRUE)

7. Enter `https://github.com/jbryer/CompStats.git` as the repository URL. The project directory name will be set to `CompStats` by default. Feel free to change this and the project directory to whatever you like.

	![Settings for Github Project](Figures/rstudio-newproject3.png?raw=TRUE)

8. To update your local copy to the latest version from Github, from the Git tab in RStudio, select `More`, then `Pull Branches`. 

	![Update](Figures/rstudio-gitpull.png?raw=TRUE)


## Alternative method without RStudio

1. Download the Github application:  
* For Mac: [http://mac.github.com/](http://mac.github.com/)
* For Windows: [http://windows.github.com/](http://windows.github.com/)

2. Go to the course website at [http://github.com/jbryer/CompStats](http://github.com/jbryer/CompStats) and click the `Clone` button (Will say `Clone in Mac` or `Clone in Windows` depending you your platform).


	![Clone](Figures/github-clone.png?raw=TRUE)

3. This will open the Github application. You will first be prompted to pick a location to save the repository.

	![Select Location](Figures/github-window1.png?raw=TRUE)

4. Once you select a location it will download the entire repository.

	![Repositories](Figures/github-window2.png?raw=TRUE)

5. Double clicking the repository will open it, from here you can see a history of changes, among other things. Clicking the `Sync Branch` button in the top right of the window will synchronize your local copy with the changes on Github.

	![Repository](Figures/github-window3.png?raw=TRUE)
