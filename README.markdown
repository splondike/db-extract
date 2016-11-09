This program suite aims to extract a subset of the data in a relational database while preserving references to other tables by including their data.

# Usage

There are three programs included, one of which does the data extraction, another that attempts to guess the foreign key relationships in your database, and a third that can draw the relationships using graphviz. Let's start with data extraction.

## Data extraction

Given the following schema, say we wanted to include all the data related to user.id = 1.

    users: id, name, created
    posts: id, user\_id, text, created

We would run the db-extract program as follows:

    ./db-extract -c config.yml users id 1

And would get a bunch of SQL as output. There is an example config file in demo-config.yml. The other command line arguments specify the table to fetch from, the column name, and the column value.

## Foreign key deduction

You will notice you have to specify the foreign key relationships inside the config file. There is a second program for deducing these relationships based on certain common naming schemes and generating an appropriate config file.

To use this program, create a config file with the database connection information, and an empty references section, and then run the program as follows:

    ./db-extract-schema -c config.yml

A new config with the deduced foreign key references will be generated.

At present the system can only handle underscore based naming schemes, and does not use foreign key constraints. Given a column called `primary_activity_id` it will look for a table containing an id column called 'primary\_activity', 'primary\_activitys', 'primary\_activities', 'activity', 'activitys', or 'activities'.

As you can end up with a lot of references in the config file, you can start to lose track of the connections between things. There is a prograph to help with that, you can use it like this:

    ./db-extract-graph config.yml | dot -Tpng -oreferences.png

And you will have a references.png file which shows the relationships in your config file. You will need to have graphviz installed (which provides the dot command).

# Building

The programs are written in Haskell and uses the Stackage system. Follow [the instructions](https://github.com/commercialhaskell/stack#how-to-install) to install that.

After that, you can build the executables using `stack build`. Near the end of the output, the program will say "Installing executable(s) in <path>". Look in path for the db-extract and db-extract-schema executables.

# Tests

The programs have some automated tests. You can run these using `stack test`.
