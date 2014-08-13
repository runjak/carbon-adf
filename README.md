About CARBON:
===
CARBON is short for 'Collaborative ARgumentation Brought ONline'.
It builds on top of DIAMOND to provide a JavaScript heavy web application that enables clients to use abstract dialectical frameworks in a collaborative context.
CARBON was initially published at [sourceforge](http://sourceforge.net/projects/carbon-adf/), but since I like my projects to be on github as well, I decided to put it here, too.
CARBON was the topic of my Bsc. Thesis, which can be found at [lips.informatik.uni-leipzig.de](http://lips.informatik.uni-leipzig.de/pub/2014-1).

How to compile and run carbon:
===

carbon uses PostgreSQL to store data permanently,
and thus it is necessary to have a PostgreSQL instance running that carbon can connect to.
To build carbon, an installation of the Haskell platform is necessary and
the development library for PostgreSQL is required as well.
On a typical Debian or Ubuntu system you can easily install all required packages via apt:

    apt-get install haskell-platform libpq-dev postgresql

If you're running a different system,
you may have to download the [Haskell Platform](http://www.haskell.org/platform/) by yourself,
or see if your system has a similar package available.

You should now have a tool called ``cabal`` in your ``$PATH``,
and we will use it in the following steps to fetch
the packages carbon depends on, and compile it afterwards.

1. Update the cabal package information using ``cabal update``.
2. From the project directory call ``cabal configure``,
which will most likely list several missing packages.
3. Fetch and install the missing packages by using ``cabal install $package1 $package2``.
To install packages for our connection to a PostgreSQL server, we would, for example, call ``cabal install HDBC HDBC-postgresql``.
4. Execute ``cabal build``.
5. Find the carbon executable in ``./dist/build/carbon/carbon``.
6. Call ``carbon nullConfig $filename`` to create an example configuration file that can be adjusted.
Note that calling ``carbon`` without parameters will list all possible command line parameters.

Now it's necessary to configure the PostgreSQL server:

1. Make sure a login and a database for carbon to use are available by executing the following steps:

    su postgres
    psql
    > CREATE ROLE exampleUser LOGIN password 'examplePassword';
    > CREATE DATABASE carbon ENCODING 'UTF8' OWNER exampleUser;

Remember to replace example{User,Password} with custom values.

2. Create the expected tables in the newly created database by ``psql carbon < ./Carbon/Backend/PostgreSQL/Design/database.sql``. Default entries for these tables will be created by carbon the first time it is started.
3. Enter the login information for PostgreSQL in a carbon configuration file.
It's no problem to edit the file to contain more newlines, spaces or tabs,
in an effort to make it easier to handle.

You can now execute ``carbon $configFile`` and visit your fresh carbon instance in a browser.
Unless changed in the config, the default port for carbon is ``8000``,
and the login for the automatically created default user is admin:admin, and should be changed soon.
