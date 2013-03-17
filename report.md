Documentation: Assignment III
=============================

Preface
-------

In this project, our focus has been on mainly 3 things:

* Decouple the domain specific code into a referentially transparent core
  module
* Write and test QuickCheck-properties that should hold for the core
* Investigate the capabilities of the STM-monad

Description of library interface
--------------------------------

### Before

The library, `Redish` (available at 
[https://github.com/honza/redish](https://github.com/honza/redish)), 
implements a subset of the `Redis` specification, as described
on [http://redis.io](http://redis.io).

`Redis` is a key-value server, which can be used for caching to improve
server performance, among other things.

The original library implements a subset of the commands described at
[http://redis.io/commands](http://redis.io/commands). When compiled and
executed, it accepts connections to a specific port, and processes any Redis
command sent to it.

The commands it supports are:

    GET key
    SET key value

A typical session with the server could be:

    SET course afp
    OK
    GET course
    afp
    SET course pfp
    OK
    GET course
    pfp

### After

Our largest improvement on the original code has been the refactoring of
domain specific code into a purely functional core library, `RedishCore.hs`.

This we have made this core library as general as possible, while the
original code only supports strings as keys and values, our improvement
supports any type of keys and any type of values.

The interface of our core library exports the following entities:

    Sized a where
      size :: a -> Int

This typeclass must be implemented by any data type that wishes to act as
keys in the database. The reason for this is that the Redish protocol
contains append operations, which involve returning the size of the
resulting value. All values must therefore have a size.

    Reply v
        
This ADT represents the possible kinds of replies specified by the Redis
protocol. It is parametrised on the type of values that the database
contains, since a reply may contain a value.

    Command k v

This ADT represents the subset of Redis commands that we have chosen to
implement. Apart from the existing `GET` and `SET` operations, we also
support `EXISTS`, which queries the database for existence of a particular
key, `DEL` which undefines the values of a specified list of keys, and
`APPEND`, which appends a value to the existing value of a specified key.

    Container v

Since the Redis protocol supports multiple types of data structures as
values, our top level type for all values is the Container type, which is
parametrised on the type of primitive values that the database contains.
Although we have not implemented any operations on other datatypes than
raw primitives, the Container type as top level values will allow the
library to be easily extended with new data structures in the future.

    DB k v

This is the actual database, which is parametrised over the type of keys
and values it contains.

Our core library only exposes two functions:

    emptyDB :: DB k v

Which gives an empty DB that contains no initial key-value bindings.

    runCommand :: (Ord k, Monoid v, Sized v) => 
      DB k v -> Command k v -> (Reply (Container v), DB k v)

This is the bread and butter of our core library. It runs a specified
command on the specified database, which yields a result and a new
database. 

We see here that keys in the database must define an ordering, which is
common in the case of efficient data structures, and nothing surprising.

The `Monoid` requirement on the type of values in the database is a direct
consequence of the `APPEND` operation in the protocol. Values must be
appendable to eachother, for which `Monoids` are a good fit, although
values need not define `mempty` to work in the database, only `mappend`.

Description of the library implementation
-----------------------------------------

### Before

The original implementation is a mixture of networking/IO-operations 
and actual modifications of an underlying data structure (a map). 

The basic idea is to have the main method listen on a socket, and fork a
handler using forkIO on any incoming connection. The main method also
defines a map as a transactional variable (TVar). This is used as the underlying
data structure for the key value storage.

The handler reads and parses incoming commands from the socket, and
executes the commands by atomically modifying the map inside the TVar
according to the specified operation, the result is then sent back over
the socket.

### After

Apart from refactoring the domain specific code into a separate core
module, as previosly described, we have also separated the remaining logic
of the application into 4 separate layers:

#### Server

This top layer listens for connections and is responsible for spawning a
handler in a new thread for every incoming connection.

#### ConnectionHandler

This layer is responsible for extracting raw data from the socket, and
passing on the resulting command input to a handler which handles the
command as an atomic transaction on the database.

#### Redish

This is the concretization of our abstract core library into a database
that uses Strings as keys and Values. It contains logic that defines how
commands are parsed from and serialized to string representations.

#### RedishCore

This is the abstract core implementation of the actual logic in the
application. The gist of the code lies within the `runCommand` function,
which runs a command in the context of a given database, and yields a
value and new database as a result of the operations performed in the
command.

#### Test suite

Since we now have a pure core library which is completely decoupled from
the IO-monad, we have implemented a QuickCheck test suite which defines a
number of properties that should hold on the `handleCommand` function that
is the main interface to the pure database.

Examples of these properties are:

*set-get*: Querying a previously defined key-value binding should always yield
the previosuly defined value.

*set-del-exists*: Deleting a previously defined key-value binding and then
queriying it should always yield a null value.

*del-app-eq-set*: Appending to an undefined key-value binding is always
the same as defining a key-value binding with the value to be appended.

In order to test these properties using QuickCheck, we have defined
Arbitrary instances of our `Container` type and `DB` type.

An arbitrary container is defined as one of the data constructors in the
container type applied to an arbitrary value of the type it contains.

An arbitrary DB is defined as a map created from an arbitiry list of
bindings between arbitrary string keys and arbitrary containers.

Since performing a number of sequential commands on a database is
inherently a stateful computation, we have designed a simple `DBTester`
monad as an alias for a `State` monad containing a database. In
conjunction with this, the `runTC` (runTestCommand) function enables 
sequential database commands and implicitly carrying the resulting database
from one command over to the next.

Every property then uses the DBTester monad to execute sequential database
queries, without explicitly keeping track of the intermediate database
representations.

When running the test suite from Cabal, we initially encountered a
problem: when looking at the test suite from lab2, the general method of reporting 
failure in the test suite seems to be calling `exitFailure`. We however do
not want to do this until all tests have completed, since otherwise we do
not know how *many* tests failed (if the test suite exits upon
encountering a failure).

In order to easily collect the results from testing multiple properties,
and then tallying these into a final result, we have designed a simple
`Checker` monad, which is essentially an alias for a `Writer` monad that
checks properties with `QuickCheck` and records the results.

Tests can then be run by composing a series of monadic `check`
computations on properties, and then running these with `runChecker` to
obtain a list of results. These results can then easily be checked for
failures.

Code analysis
-------------

The following code snippet showcases how commands to the core library are
carried out:

    runCommand :: (Ord k, Monoid v, Sized v) => 
      DB k v -> Command k v -> (Reply (Container v), DB k v)
    runCommand db@(DB mdb) cmd = case cmd of
      (Get k) -> case lookup k mdb of
        Just v -> (BulkRep v, db)
        Nothing -> (NBulkRep, db)
      (Set k v) -> (StatRep "OK", DB $ insert k (Raw v) mdb)
      (Exists k) -> (IntRep $ maybe 0 (const 1) (lookup k mdb), db)
      (Del ks) -> let mdb' = (foldr delete mdb ks) 
                  in (IntRep (size mdb - size mdb'), DB mdb')
      (Append k va) -> case lookup k mdb of
        Just (Raw v) -> let v' = (v `mappend` va) 
                        in (IntRep $ size v', DB $ insert k (Raw v') mdb)
        Just _ -> (IntRep $ size va, DB $ insert k (Raw va) mdb)
        Nothing -> (IntRep $ size va, DB $ insert k (Raw va) mdb)

Notice for example how the monoidal property of the values in the database
allows us to easily implement the `APPEND` operation.

The following code snippet illustrates the intersection between IO-monadic
code, and code that is run as an atomic transaction in the STM-monad.

    processCommand :: Handle -> TVar RedishDB -> IO ()
    processCommand handle tdb = do
        input <- hGetLine handle
        reply <- atomically $ handleCommand input tdb
        hPutStrLn handle reply
        processCommand handle tdb

    handleCommand :: String -> TVar RedishDB -> STM String
    handleCommand input tdb = do
      db <- readTVar tdb
      let (rep, db') = interpretCommand db input
      writeTVar tdb db'
      return rep

The following code snippet illustrates how we make use of existing
arbitrary definitions in order to build an arbitrary database:

    instance (Arbitrary a) => Arbitrary (Container a) where
      arbitrary = arbitraryContainer

    arbitraryContainer :: (Arbitrary a) => Gen (Container a)
    arbitraryContainer = oneof 
      [ liftM Raw arbitrary
      , liftM List arbitrary ]

    instance (Ord k, Arbitrary k, Arbitrary v) => Arbitrary (DB k v) where
      arbitrary = liftM (DB . Map.fromList) arbitrary

The following code snippet illustrates how we use the State monad to carry
out sequential database commands in our test cases.

    type DBTester a = State TestDB a
    type TestDB = DB String String
    type TestCommand = Command String String
    type TestReply = Reply (Container String)

    runTC :: TestCommand -> DBTester TestReply
    runTC c = do
      db <- get
      let (r, db') = runCommand db c
      put db'
      return r

    runDBT :: TestDB -> DBTester a -> a
    runDBT = flip evalState

Here is an example of a property that should hold on all databases:
performing an append on an undefined value is the same as defining it.
Since we test on arbitrary databases, we use the delete command to make
sure that the binding is initially undefined.

    prop_del_app_eq_set :: String -> String -> TestDB -> Bool
    prop_del_app_eq_set k v db = runDBT db $ do
      runTC $ Del [k]
      runTC $ Append k v
      r1 <- runTC $ Get k
      runTC $ Set k v
      r2 <- runTC $ Get k
      return $ r1 == r2 

Finally, take a look at how we solved the problem of collecting the
results from checking multiple properties using the `Writer` monad.

    newtype Checker a = Checker { unChecker :: WriterT [Result] IO a }
      deriving ( Monad, MonadWriter [Result], MonadIO )

    runChecker :: Checker () -> IO [Result]
    runChecker = liftM snd . runWriterT . unChecker

    check :: Testable prop => prop -> Checker ()
    check p = liftIO (quickCheckResult p) >>= tell . (:[])

    checks :: Checker ()
    checks = do
      check prop_set_get
      check prop_get_get
      {- ... -}

    main :: IO ()
    main = runChecker checks >>= \rs ->
      unless (all isSuccess rs) exitFailure
