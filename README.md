# log-watchdog

A Clojure utility that watches log files for lines matching configured regular expressions and shows balloon notifications in system tray. The user needs to acknowledge the alerts, otherwise the notifications will periodically reappear. Useful companion for everyday development/testing work.

The watched files can be organized into groups and alerts can be acknowledged at the level of a file group or of an individual file. Useful for checking multiple different applications or of multiple instances of the same application.


## Configuration

The application is fuly configured via file *configuration.edn*. This file needs to be in the current working directory when running the application.
Check out the included *configuration.edn* config for comments.


## Usage

Compile and run:

    $ cd <log-watchdog-directory>
    $ lein uberjar
    $ java -jar target/uberjar/log-watchdog-*.jar

Note: Make sure to have file "configuration.edn" in current working directory when running the application.
