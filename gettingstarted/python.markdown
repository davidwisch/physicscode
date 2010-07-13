---
layout: default
title: Installing/Configuring Python
---

# Installing/Configuring Python

Python is a cross-platform language meaning that it can run on a variety of operating systems.

**Please select the instructions for your system:**

* [Microsoft Windows](#windows-install)
* [Apple OSX](#osx-install)
* [Ubuntu Linux](#ubuntu-install)

<a name="windows-install"></a>
## Microsoft Windows

These instructions that you're using Python v2.7.x.

1. Goto [http://python.org/download/](http://python.org/download/) and download the appropriate package for your system.
2. Install the package that you downloaded.

**Test the installation**

Open up a command prompt and type in 'python'.  Hopefully you should see the following:

{% image gettingstarted/python/windowstest %}

If instead you see, "'python' is not recognized as an internal or external command..." then you may need to modify your PATH variable.

**To modify your PATH variable:**

1. Right click on 'My Computer'
2. Select 'Properties'
3. Select the 'Advanced' tab
4. Click the 'Environment Variables' button
5. In the 'System Variables' section, findn PATH and click the 'Edit' button
6. After the text in the user 'Variable value' box, put the follpwing: C:\Python27

You may need to Logout/Login before the changes take effect.  You should now be ready to use Python.

<a name="osx-install"></a>
##  Apple OSX

You're in luck!  Python already is installed on your system.  If you'd like to install a newer version see [http://www.python.org/download/mac/](http://www.python.org/download/mac/).

<a name="ubuntu-install"></a>
##Ubuntu Linux

You're in luck!  Python should already be installed on your system.  Incase it isn't, simply run:

{% highlight bash %}
sudo apt-get install python
{% endhighlight %}

from the commant line to install.
