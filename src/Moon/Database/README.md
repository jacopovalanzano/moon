# Database Module

The SQL database module is based on the [mysql-simple](http://hackage.haskell.org/package/mysql-simple) framework and 
provides a set of functions for queries and database operations.


## Example usage

```haskell
-- moon/app/Main.hs
main :: IO ()
main = do

    ...

    -- Initialize the Database with connection details
    let db = Moon.Database.SQLDatabase { dbConnection = undefined, dbName = "test", dbHost = "127.0.0.1", dbUser = "root", dbPassword = "toor" }

    -- Open a connection
    db' <- open db

    -- Execute a query ( returns an integer Int64 )
    result <- exe db' "DELETE FROM cache WHERE expiration = 1716555478;" :: IO Int64

    -- Print the result
    print result

    -- Execute one more query ( returns a list of tuples )
    result <- ask db' "SELECT id,name,location,rands,salt FROM user WHERE 1=1;" :: IO [(Int, String, String, String, String)]

    -- Print the result
    print result

    -- Close the connection
    quit db'
```