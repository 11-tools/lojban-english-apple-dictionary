Lojban-English Plugin for Apple Dictionary
==========================================

Created in 2009 by Joshua Choi.

About
-----

This is a plugin for Apple Dictionary: it adds a Lojban dictionary in English. The plugin works in Apple Dictionary 2 in Mac OS X 10.5 only.

Currently, the dictionary only has *cmavo* and *gismu*, which I'll probably rectify later. If you're anxious, feel free to fork [the project on GitHub](http://github.com/joshua-choi/lojban-english-apple-dictionary)—it's easy.

How to install
--------------

If you're reading this from the Internet, download and mount the latest disk image from [the "Downloads" page of the project](http://github.com/joshua-choi/lojban-english-apple-dictionary/downloads).

Once you've mounted the disk image, all you have to do is drag-and-drop the folder in the image into the disk image's alias to your Dictionary folder.

Once you restart Apple Dictionary, the Lojban-English dictionary should be visible in the toolbar and preferences.

How to uninstall
----------------

Quit Apple Dictionary if it's open, and then nagivate in Finder to your Dictionary folder, which is at ~/Library/Dictionaries/. The folder that you dragged from the disk image, "Lojban-English Dictionary.dictionary", should be there. Move it to the Trash, and then the plugin is uninstalled.

The GitHub project
------------------

If you're reading this from the GitHub project page, the tree you're looking at is the raw source code of the dictionary, before it's built. If you want a user-ready, already-built version, read the "How to install" section above.

You don't need to read this section if you don't want to edit and reprogram the dictionary.

Before you mess around, you should really read the [Apple Dictionary Services Programming Guide](http://developer.apple.com/documentation/userexperience/Conceptual/DictionaryServicesProgGuide/Introduction/Introduction.html).

The script inside, "convert-to-xml.clj", is a Clojure script. It converts the *plain text definition files* into the *XML file*. You *don't need to do this* unless you want to rebuilt the XML file. To run it, you need the Java Virtual Machine—which shouldn't be a problem—and [Clojure](http://www.clojure.org). Also, you'll need the [clojure-contrib user libraries](http://www.github.com/kevinoneill/clojure-contrib/).

In order to run the script, your terminal needs to be inside the "src" folder. After you make a new XML file, run "make" and see if you get any errors. If no errors appear, run "make install; make clean" to install the dictionary in your Apple Dictionary and test it, also deleting the now-unneeded intermediate folder that appeared.

License
-------

The Clojure code inside the "convert-to-xml.clj" file in the "src" folder is licensed under the [GNU General Public License](http://creativecommons.org/licenses/GPL/2.0/).

The information in the dictionary and the word lists are in the public domain.

The dictionary's etymologies and word origins are thanks to [Mr./Ms. Mublin](https://www.dealloc.org/~mublin/), and were placed in the public domain.

"Lojban" is a trademark of the Logical Language Group.

