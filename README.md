# log-watchdog

A Clojure utility that watches log files for lines matching configured regular expressions and shows balloon notifications in system tray. The user needs to acknowledge the alerts, otherwise the notifications will periodically reappear.


## Usage

Compile and run:

    $ lein uberjar
    $ java -jar target/uberjar/log-watchdog-*.jar

Note: Make sure to have file "configuration.edn" in current working directory when running the application.


## Options

The application is fuly configured via file "configuration.edn" that needs to be in the current working directory when running the application.


## What is next

* alert aging and cleaning
* idiomatic use of zippers
* ...

