#
**Intern Stats**

hi interns!! ヾ(＾∇＾)

This repository is for all the stats code! 

I recommend you use RStudio. It allows you to see your variables, datasets, plots, and script in an easier to use format.

##
HOW TO USE:

To open files:
1. Use R -- can be kind of confusing but it works
2. Use RStudio -- a lot less confusing to use. Allows you to see variables/files/working directory.

You can either:
1. Download the code file directly
2. Copy and paste the code into your IDE/editor of choice
3. git pull/fetch if you have Git Bash installed

PLEASE do not commit to the master branch unless you know the code works/you are making major changes. Create a new branch if you have different versions.

Running code:

You can:
1. Click run at the top to run the line you're on.
2. Copy/paste lines into the terminal to run them.
3. Use Ctrl+Enter to run each line you're on.

##
Current files
1. *Propensity Matching*
    - Commented file
        - This file has comments for every line so you can be walked through it.
    - Uncommented file
        - This file only has one comment so it's cleaner to copy paste it. Use this file only if you understand what the code does.
2. *Kaplan Meier*
    - Commented file
3. *statfunc.R*
   - This has every function you'll need post match.
4. *stats scope*
   - This file has the implementation of each function in stat func so you know how to use it.

##
Debugging + tips
1. *General*
    - Make sure you have a CSV file, not an xlsx!!
    - Check your working directory to make sure files are being pulled from the right place. You can do this with Ctrl+Shift+H.
    - Check the headers for each code line to make sure they line up with the headers for your dataframe.
    - Check header formatting. Sometimes they're read in weird into R, so it's helpful to keep your csv file headers simple, as well as get rid of any spaces.
    - Check and make sure you're looking at the most updated version of your file. If you edit your file, make sure you save it and rerun the line to read your data in.
    - Make sure you're only reading in the data you need -- you might have to trim your file to get rid of null rows or columns that might be messing you up.
2. *Propensity Matching*
    - Data formatting 
        1. Data
