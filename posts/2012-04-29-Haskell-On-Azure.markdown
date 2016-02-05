---
title: Haskell On Azure
author: Phil Freeman
date: 2012/04/29
description: Running HappStack in an Azure worker role instance, and using the tablestorage cabal package.
tags: Haskell, Azure
---

Today, as a change of topic, I\'m going to write about getting a Haskell web application to run on Windows Azure.

Azure provides storage services (the table, queue and blob storage services) exposing REST APIs. I recently uploaded my first cabal package tablestorage to HackageDB, which is a simple wrapper for the web methods of the table storage REST service.

I\'ve chosen to use a combination of the Happstack web server and blaze-html to build a very simple note-taking web application, but it should be possible to switch out the particular web server easily.

~~~{.text}
module Main where

import Data.Maybe
import Control.Monad
import Control.Monad.Trans
import System.Time
import System.Directory
import Happstack.Server
import qualified Text.Blaze.Html4.Strict as H
import qualified Text.Blaze.Html4.Strict.Attributes as A
import Network.TableStorage
import Text.Blaze ((!), toValue)
~~~

Let\'s define the account we are going to be using. An account consists of a service endpoint, an account name and the account\'s secret key.

For now, we\'ll use the development account, so you will need to have the Windows Azure storage emulator running on your development machine.

~~~{.text}
account :: Account
account = developmentAccount 
~~~

When it is time to deploy the account to staging/production, we can replace `developmentAccount` with a call to defaultAccount with the account information copied from the Azure management portal.

The note-taking appplication will have simple features: there will be multiple "notebooks", the ability to list the most recent notes in a notebook, and the ability to add a new note to a notebook. We will shard the notes as necessary based on the notebook. In the table storage model, this means that the notebook being used will be determined by the Partition Key of the note entities.

We need the ability to generate a new key. We would like note IDs to be approximately sorted by insertion date. In order that more recent notes will be displayed first, we will generate a key based on the current time, subtracting the seconds component of the current time from a large value to reverse the order of the generated keys.

~~~{.text}
newId :: IO String
newId = do
  (TOD seconds picos) <- getClockTime
  return $ show (9999999999 - seconds) ++ show picos
~~~

With that, we can implement the code which will add a new note. This will be implemented as a POST method which accepts its parameters as form-encoded data in the request body.

A new key is generated, and then the insertEntity IO action is used to add the newly created note entity to the notes table, along with the author and text properties as string columns.

If this is successful, we perform a redirect-and-get to display the newly added note to the user. If the insert is not successful, then we respond with 500 - Internal Server Error.

~~~{.text}
postNote :: ServerPartT IO Response
postNote = do 
  methodM POST
  tmp <- liftIO getTemporaryDirectory 
  decodeBody $ defaultBodyPolicy tmp 0 1000 1000
  text <- look "text"
  author <- look "author"
  partition <- look "partition"
  result <- liftIO $ do
    id <- newId
    let entity = Entity { entityKey = EntityKey { ekPartitionKey = partition, 
                                                  ekRowKey = id }, 
                          entityColumns = [ ("text", EdmString $ Just text),
                                            ("author", EdmString $ Just author)] }
    insertEntity account "notes" entity
  case result of
    Left err -> internalServerError $ toResponse err
    Right _ -> seeOther ("?partition=" ++ partition) $ toResponse ()
~~~

To list the recent notes in the selected notebook, we use queryEntities to query the top 10 entities from the notes table, filtering based on the partition key.

~~~{.text}
getNotes :: ServerPartT IO Response
getNotes = do
  methodM GET
  partition <- look "partition" `mplus` return "default"
  let query = defaultEntityQuery { eqPageSize = Just 10,
                                   eqFilter = Just $ CompareString "PartitionKey" Equal partition }
  result <- liftIO $ queryEntities account "notes" query
  case result of
    Left err -> internalServerError $ toResponse err
    Right notes -> ok $ setHeader "Content-Type" "text/html" $ toResponse $ root partition notes
~~~

This demonstrates a very basic query, but the tablestorage package supports more complicated queries including multiple filters.

If the query fails, then we return response code 500. If it is successful, then we return a HTML page built using blaze-html. The is the content of the root function:

~~~{.text}
root :: String -> [Entity] -> H.Html
root partition notes = H.html $ do
  H.head $ 
    H.title $ H.toHtml "Notes"
  H.body $ do
    H.h1 $ H.toHtml "Add Note"
    H.form ! A.method (toValue "POST") $ do
      H.input ! A.type_ (toValue "hidden") ! A.name (toValue "partition") ! A.value (toValue partition)
      H.div $ do
        H.label ! A.for (toValue "text") $ H.toHtml "Text: "
        H.input ! A.type_ (toValue "text") ! A.name (toValue "text")
      H.div $ do
        H.label ! A.for (toValue "author") $ H.toHtml "Author: "
        H.input ! A.type_ (toValue "text") ! A.name (toValue "author")
      H.div $
        H.input ! A.type_ (toValue "submit") ! A.value (toValue "Add Note")
    H.h1 $ H.toHtml "Recent Notes"
    H.ul $ void $ mapM displayNote notes
~~~

The first part of the page contains a form which will allow the user to post a new note. It contains text boxes for the text and author arguments. The partition key is provided as a hidden field on the form, which the user can change by editing the query string.

The last part of the page lists the most recent ten notes in the selected notebook (partition key). Each note is rendered using the displayNote function:

~~~{.text}
displayNote :: Entity -> H.Html
displayNote note = fromMaybe (return ()) $ do
  text <- edmString "text" note
  author <- edmString "author" note
  return $ H.li $ do
    H.toHtml "'"
    H.toHtml text
    H.toHtml "'"
    H.i $ do 
      H.toHtml " says "
      H.toHtml author
~~~

Here, we use the `edmString` helper function to extract a string-valued column from the list of columns returned by the query. This `Maybe`-valued function returns Just a string if the named column is indeed string valued, and Nothing if the column is missing or has a different type. The tablestorage package provides corresponding functions for the other column types.

Finally, we can create a web application using the `getNotes` and `postNote` routes:

~~~{.text}
routes :: [ServerPartT IO Response]
routes = [ getNotes, postNote ]
~~~

The `main` function performs the setup step of creating the notes table if the table does not already exist. The helper function `createTableIfNecessary` simplifies this check:

~~~{.text}
main :: IO ()
main = do 
  result <- createTableIfNecessary account "notes"
  case result of
    Left err -> putStrLn err
    Right _ -> simpleHTTP nullConf $ msum routes
~~~

The web application can be deployed to the development fabric by using the `cspack` and `csrun` command line utilities.

We need to add a few basic configuration files. The web server will run in a worker role, which we set up in the ServiceDefinition.csdef file. We need to add a worker role definition, something like the following:

~~~
-- <ServiceDefinition name="Notes" xmlns="http://schemas.microsoft.com/ServiceHosting/2008/10/ServiceDefinition">
--   <WorkerRole name="WebServer" vmsize="Small">
--     <Runtime>
--       <EntryPoint>
--         <ProgramEntryPoint commandLine="Main.exe" setReadyOnProcessStart="true" />
--       </EntryPoint>
--     </Runtime>
--     <Endpoints>
--       <InputEndpoint name="happstackEndpoint" protocol="tcp" port="80" />
--     </Endpoints>
--   </WorkerRole>
-- </ServiceDefinition>
~~~

The `ServiceConfiguration.*.cfg` files should contain a role section with the same worker role name:

~~~
-- <Role name="WebServer">
--   <ConfigurationSettings />
--   <Instances count="1" />
-- </Role>
~~~

After the web application is built, the `Main.exe` executable is placed in the WebServer role subdirectory, and the package can be built for development deployment using

~~~
-- cspack /copyOnly ServiceDefinition.csdef
~~~

The package can then be deployed locally using

~~~
-- csrun Notes.csx ServiceConfiguration.Local.cscfg
~~~

You should be able to add new notes, display existing notes, and switch notebooks by editing the query string.

When you are ready to deploy to Azure in staging or production (you will need to replace the development storage account in the code with the account credentials copied from the management portal), you can build a cspkg file for upload to Azure with

~~~
-- cspack ServiceDefinition.csdef
~~~
