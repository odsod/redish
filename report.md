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
