Lojban-English Plugin for Apple Dictionary
==========================================

Created in 2009 by Joshua Choi.

Works in Apple Dictionary 2 in Mac OS X 10.5 only.

How to install
--------------

If you're reading this from the Internet, download and mount the latest disk image from [the "Downloads" page of the project](http://github.com/joshua-choi/lojban-english-apple-dictionary/downloads).

Once you've mounted the disk image, all you have to do is drag-and-drop the folder in the image into the disk image's alias to your Dictionary folder.

Once you restart Apple Dictionary, the Lojban-English dictionary should be visible in the toolbar and preferences.

How to uninstall
----------------

Quit Apple Dictionary if it's open, and then nagivate in Finder to your Dictionary folder, which is at ~/Library/Dictionaries/. The folder that you dragged from the disk image, "Lojban-English Dictionary.dictionary", should be there. Move it to the Trash, and then the plugin is uninstalled.

License
-------

The Clojure code inside the "convert-to-xml.clj" file in the "src" folder is licensed under the [GNU General Public License](http://creativecommons.org/licenses/GPL/2.0/).

The information in the dictionary and the word lists are in the public domain.

"Lojban" is a trademark of the Logical Language Group.
