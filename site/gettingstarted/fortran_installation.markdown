---
layout: default
title: Installing/Configuring Fortran
---

# Installing/Configuring Fortran

**Please select the instructions for your system:**

* [Microsoft Windows](#windows-install)
* [Apple OSX](#osx-install)
* [Ubuntu Linux](#ubuntu-install)

<a name="windows-install"></a>
## Microsoft Windows

1. Download [http://ftp.g95.org/g95-MinGW.exe](http://ftp.g95.org/g95-MinGW.exe).
2. Run the executable that you just downloaded selecting "yes" to all of its questions.

You may need to restart to complete the installation.

**Test the installation**

Open up a command prompt and type in 'g95'.  Hopefully you should see the following output:

`g95: no input files`

If instead you see, "'g95' is not recognized as an internal or external command..." then you may need to modify your PATH variable.

**To modify your PATH variable:**

1. Right click on 'My Computer'
2. Select 'Properties'
3. Select the 'Advanced' tab
4. Click the 'Environment Variables' button
5. In the 'System Variables' section, find PATH and click the 'Edit' button
6. After the text in the user 'Variable value' box, put the following: C:\$G95_HOME\bin (where $G95_HOME is where you installed G95 (usually the same place you downloaded it if you accept all of the defaults)).

If you downloaded G95 to your Desktop and installed it there, the text to append to your PATH is:

* **Windows XP**: C:\Documents and Settings\\\[username\]\Desktop\g95\bin
* **Windows Vista/7**: C:\Users\\\[username\]\Desktop\g95\bin

where \[username\] should be replaced with *your* username.

You should now be ready to use G95 to compile your Fortran 90/95 files.

<a name="osx-install"></a>
##  Apple OSX

**RJ You should fill this section out**

<a name="ubuntu-install"></a>
##Ubuntu Linux

A Fortran compiler is not installed by default on Ubuntu systems.  To install gfortran, simply run:

{% highlight bash %}
sudo apt-get install gfortran
{% endhighlight %}

from the command line to install.

You can choose to use G95 instead of gfortran but you'll have install it manually.  Visit the [G95 Project](http://www.g95.org/) for more information.
