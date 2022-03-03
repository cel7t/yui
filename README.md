# yui

A personal Discord bot written in Clojure!

## Usage

Edit the commented parameters in config.edn to begin.
Any string that contains yui, such as "Yui-chan," "yui", "YUI" will invoke her.
The second option has to be a command name.
Command name list can be accessed via yui help.

## Notes
- is-mod can only check for one person's id. Moderator role checking is not yet added
- since yui is meant to be run from heroku, files aren't used for dailies/coins. due to this, the process can take several seconds, as it makes use of discord channels to store these values
- make yui say 1 1 in the score channel and 1 1 in the daily channel and pin the message. Yui will now use this message to keep track of the score. I highly recommend using a separate server to store these messages. You can edit out the 1 1s later
- yui search is still buggy, it might return "From ," as the result if duckduckgo can't find a quicksearch result

## License

Copyright © 2021 Sarthak Shah

This program and the accompanying materials are made available under the
terms of the Eclipse Public License 2.0 which is available at
http://www.eclipse.org/legal/epl-2.0.
