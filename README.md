# log-watchdog

A Clojure utility useful for everyday development and/or testing work. The utility watches log files for lines matching configured regular expressions and shows alerts in form of balloon notifications in system tray. The user needs to acknowledge the alerts, otherwise the notifications will periodically reappear.

The watched files can be organized into groups and alerts can be acknowledged at the level of a file group or of an individual file. Useful for keeping track of alerts in log files of different applications or of multiple instances of the same application.


## Configuration

The application is fuly configured via file *configuration.edn*. This file needs to be in the current working directory when running the application.
To configure files you want to check or to customize behavior, check out the included *configuration.edn* config file.


## Usage

Compile and run:

    $ cd <log-watchdog-directory>
    $ lein uberjar
    $ java -jar target/uberjar/log-watchdog-*.jar

Note: Make sure to have *configuration.edn* file in current working directory when running the application.
