-- derived from: https://github.com/rcook/aws-via-haskell/tree/master/dynamodb
{-# LANGUAGE FlexibleContexts #-}
-- All of amazonka APIs use Data.Text.Text by default which is nice
{-# LANGUAGE OverloadedStrings #-}
-- Allows record fields to be expanded automatically
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Main (main) where

import           AWSViaHaskell
import           Control.Exception.Lens (handling)
import           Control.Monad (void, when)
import qualified Data.HashMap.Strict as HashMap (fromList, lookup)
import           Data.List.NonEmpty (NonEmpty(..))
import           Data.Text (Text)
import           DynamoDBImports
import           System.Directory (getHomeDirectory)
import           System.FilePath ((</>))

wrapAWSService 'dynamoDB "DDBService" "DDBSession"

newtype TableName = TableName Text deriving Show

-- Creates a table in DynamoDB and waits until table is in active state
doCreateTableIfNotExists :: TableName -> DDBSession -> IO ()
doCreateTableIfNotExists (TableName tn) = withAWS $ do
    newlyCreated <- handling _ResourceInUseException (const (pure False)) $ do
        void $ send $ createTable
                        tn
                        (keySchemaElement "counter_name" Hash :| [])
                        (provisionedThroughput 5 5)
                        & ctAttributeDefinitions .~ [ attributeDefinition "counter_name" S ]
        return True
    when newlyCreated (void $ await tableExists (describeTable tn))

-- Deletes a table in DynamoDB if it exists and waits until table no longer exists
doDeleteTableIfExists :: TableName -> DDBSession -> IO ()
doDeleteTableIfExists (TableName tn) = withAWS $ do
    deleted <- handling _ResourceNotFoundException (const (pure False)) $ do
        void $ send $ deleteTable tn
        return True
    when deleted (void $ await tableNotExists (describeTable tn))

-- Puts an item into the DynamoDB table
doPutItem :: TableName -> Int -> DDBSession -> IO ()
doPutItem (TableName tn) value = withAWS $ do
    void $ send $ putItem tn
                    & piItem .~ item
    where item = HashMap.fromList
            [ ("counter_name", attributeValue & avS .~ Just "my-counter")
            , ("counter_value", attributeValue & avN .~ Just (intToText value))
            ]

-- Updates an item in the DynamoDB table
doUpdateItem :: TableName -> DDBSession -> IO ()
doUpdateItem (TableName tn) = withAWS $ do
    void $ send $ updateItem tn
                    & uiKey .~ key
                    & uiUpdateExpression .~ Just "ADD counter_value :increment"
                    & uiExpressionAttributeValues .~ exprAttrValues
    where
        key = HashMap.fromList
            [ ("counter_name", attributeValue & avS .~ Just "my-counter")
            ]
        exprAttrValues = HashMap.fromList
            [ (":increment", attributeValue & avN .~ Just "1" )
            ]

-- Gets an item from the DynamoDB table
doGetItem :: TableName -> DDBSession -> IO (Maybe Int)
doGetItem (TableName tn) = withAWS $ do
    result <- send $ getItem tn
                        & giKey .~ key
    return $ do
        valueAttr <- HashMap.lookup "lf_username" (result ^. girsItem)
        valueNStr <- valueAttr ^. avN
        parseInt valueNStr
    where key = HashMap.fromList
            [ ("lf_username", attributeValue & avS .~ Just "my-counter")
            ]

main :: IO ()
main = do
    let tableName = TableName "cla-dev-users"

    homeDir <- getHomeDirectory
    let conf = awsConfig (AWSRegion Ohio)
                & awscCredentials .~ (FromFile "test-creds" $ homeDir </> ".aws" </> "credentials")

    ddbSession <- connect conf dynamoDBService

    --putStrLn "DeleteTableIfExists"
    --doDeleteTableIfExists tableName ddbSession

    --putStrLn "CreateTableIfNotExists"
    --doCreateTableIfNotExists tableName ddbSession

    --putStrLn "PutItem"
    --doPutItem tableName 1234 ddbSession

    --putStrLn "UpdateItem"
    --doUpdateItem tableName ddbSession

    putStrLn "GetItem"
    counter <- doGetItem tableName ddbSession
    print counter

    putStrLn "Done"
