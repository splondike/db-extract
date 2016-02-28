module ExtractSchemaTest where

import Test.Framework (testGroup)
import Test.Framework.Providers.HUnit
import Test.HUnit

import Data.String (fromString)
import Data.Either (isLeft)
import qualified Data.ByteString as BS
import qualified Data.Set as S
import qualified Control.Concurrent.MVar as MV

import Config(ForeignKey(..))
import DataLayer.Types (TableSchema(..), TableColumn(..), TableFKConstraint(..))
import qualified ExtractSchema.Program as P
import qualified DataLayer as DL

allTests = testGroup "ExtractData Tests" [
   testCase "Can disable FK inference" testTogglesInference,
   testCase "Finds foreign keys by *_id with pluralized tables" testFindsPluralByIdSuffix,
   testCase "Finds foreign keys by *_id with *ies tables" testFindsByIesTables,
   testCase "Finds foreign keys by *_id with singular tables" testFindsSingularByIdSuffix,
   testCase "Prefers longest table match" testPrefersLongestMatch,
   testCase "Finds multiple in one table" testFindsMultipleInOneTable,
   testCase "Ignores column case" testIgnoresTextCase,
   testCase "Handles missing table matches with *_id suffix" testHandlesMissing,
   testCase "Handles missing id on referenced table" testHandlesMissingId,
   testCase "Handles self references" testHandlesSelfReferences,
   testCase "Includes explicit foreign keys" testIncludesForeignKeys]

testTogglesInference = do
   result <- P.processDatabase db False
   assertEqual "Finds expected foreign keys" expected result
   where
      db = buildFromMap [
         ("blogs", ["id", "author_id", "title"]),
         ("authors", ["id", "org_role_id", "name"])]
      expected = S.fromList []

testFindsPluralByIdSuffix = do
   result <- P.processDatabase db True
   assertEqual "Finds expected foreign keys" expected result
   where
      db = buildFromMap [
         ("blogs", ["id", "author_id", "title"]),
         ("authors", ["id", "org_role_id", "name"]),
         ("org_roles", ["id", "name"])]
      expected = S.fromList [
         ForeignKey "blogs" "author_id" "authors" "id",
         ForeignKey "authors" "org_role_id" "org_roles" "id"]

testFindsByIesTables = do
   result <- P.processDatabase db True
   assertEqual "Finds expected foreign keys" expected result
   where
      db = buildFromMap [
         ("todos", ["id", "activity_id"]),
         ("activities", ["id", "name"])]
      expected = S.fromList [
         ForeignKey "todos" "activity_id" "activities" "id"]

testFindsSingularByIdSuffix = do
   result <- P.processDatabase db True
   assertEqual "Finds expected foreign keys" expected result
   where
      db = buildFromMap [
         ("blog", ["id", "author_id", "title"]),
         ("author", ["id", "org_role_id", "name"]),
         ("org_role", ["id", "name"])]
      expected = S.fromList [
         ForeignKey "blog" "author_id" "author" "id",
         ForeignKey "author" "org_role_id" "org_role" "id"]

testPrefersLongestMatch = do
   result <- P.processDatabase db True
   assertEqual "Finds expected foreign keys" expected result
   where
      db = buildFromMap [
         ("authors", ["id", "primary_org_role_id", "secondary_org_role_id", "name"]),
         ("contributors", ["id", "primary_role_id", "secondary_role_id", "name"]),
         ("org_roles", ["id", "name"]),
         ("roles", ["id", "name"])]
      expected = S.fromList [
         ForeignKey "authors" "primary_org_role_id" "org_roles" "id",
         ForeignKey "authors" "secondary_org_role_id" "org_roles" "id",
         ForeignKey "contributors" "primary_role_id" "roles" "id",
         ForeignKey "contributors" "secondary_role_id" "roles" "id"]

testFindsMultipleInOneTable = do
   result <- P.processDatabase db True
   assertEqual "Finds expected foreign keys" expected result
   where
      db = buildFromMap [
         ("blogs", ["id", "author_id", "asset_id", "title"]),
         ("authors", ["id", "name"]),
         ("assets", ["id", "name"])]
      expected = S.fromList [
         ForeignKey "blogs" "author_id" "authors" "id",
         ForeignKey "blogs" "asset_id" "assets" "id"]

testIgnoresTextCase = do
   result <- P.processDatabase db True
   assertEqual "Finds expected foreign keys" expected result
   where
      db = buildFromMap [
         ("blogs", ["id", "author_ID", "title"]),
         ("authors", ["id", "org_role_id", "name"]),
         ("org_roles", ["ID", "name"])]
      expected = S.fromList [
         ForeignKey "blogs" "author_ID" "authors" "id",
         ForeignKey "authors" "org_role_id" "org_roles" "ID"]

testHandlesMissing = do
   result <- P.processDatabase db True
   assertEqual "Finds expected foreign keys" expected result
   where
      db = buildFromMap [
         ("blogs", ["id", "author_id", "title"])]
      expected = S.fromList []

testHandlesMissingId = do
   result <- P.processDatabase db True
   assertEqual "Finds expected foreign keys" expected result
   where
      db = buildFromMap [
         ("blogs", ["id", "author_id","title"]),
         ("authors", ["notid", "name"])]
      expected = S.fromList []

testHandlesSelfReferences = do
   result <- P.processDatabase db True
   assertEqual "Finds expected foreign keys" expected result
   where
      db = buildFromMap [
         ("blogs", ["id", "blog_id","title"])]
      expected = S.fromList [
         ForeignKey "blogs" "blog_id" "blogs" "id"]

testIncludesForeignKeys = do
   result <- P.processDatabase db True
   assertEqual "Finds expected foreign keys" expected result
   where
      db = buildFromMapWithFks cols fks
      cols = [("blogs", ["id", "author_id", "title"]),
         ("authors", ["id", "name"]),
         ("admirers", ["id", "author_name"])]
      fks = [("admirers", [("author_name", "authors", "name")])]
      expected = S.fromList [
         ForeignKey "blogs" "author_id" "authors" "id",
         ForeignKey "admirers" "author_name" "authors" "name"]

buildFromMap :: [(String, [String])] -> DL.DBFetcher
buildFromMap database = buildFromMapWithFks database []

buildFromMapWithFks :: [(String, [String])] -> 
                       [(String, [(String, String, String)])] ->
                       DL.DBFetcher
buildFromMapWithFks database fks = DL.DBFetcher{
                           DL.getByColumn = \_ _ -> return $ Left "Not implemented",
                           DL.testConnection = return True,
                           DL.escapeString = return . id,
                           DL.getSchema = gs
                        }
   where
      gs = return $ Right schemas
      schemas = map makeSchema database
      makeSchema (name, colNames) = TableSchema (fromString name) (map makeCol colNames) (getFks name)
      getFks name = case lookup name fks of
                         Just triples -> map makeFk triples
                         Nothing -> []
      makeFk (lc, rt, rc) = TableFKConstraint (fromString lc) (fromString rt) (fromString rc)
      makeCol = TableColumn . fromString
